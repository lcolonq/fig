module Fig.Emulator.GB.Component.Video where

import Fig.Prelude
import Prelude (error)

import qualified Foreign.Ptr as Ptr
import qualified Foreign.Storable as St

import Control.Monad (unless)

import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import qualified SDL

import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.Bus

newtype VideoError = VideoError Text
  deriving Show
instance Exception VideoError
instance Pretty VideoError where
  pretty (VideoError b) = mconcat
    [ "video error: "
    , b
    ]

screenWidth :: Word16
screenWidth = 160

screenHeight :: Word16
screenHeight = 144

newtype Framebuffer = Framebuffer
  { fbSurface :: SDL.Surface
  }

initializeFramebuffer :: IO Framebuffer
initializeFramebuffer = do
  s <- liftIO $ SDL.createRGBSurface
    (SDL.V2 (fromIntegral screenWidth) $ fromIntegral screenHeight)
    SDL.RGBA8888
  pure Framebuffer
    { fbSurface = s
    }

logTilemap :: V.Vector Word8 -> IO ()
logTilemap v = do
  let base = 0x9800 - 0x8000
  bytes <- forM ([0..(32 * 32)] :: [Int]) \i -> do
    case v V.!? (base + i) of
      Nothing -> error "unreachable"
      Just a -> pure a
  log $ "tilemap:" <> tshow bytes

blitPixel :: Word32 -> (Word8, Word8) -> Framebuffer -> IO ()
blitPixel c (x, y) fb = do
  SDL.lockSurface $ fbSurface fb
  (base :: Ptr.Ptr Word32) <- Ptr.castPtr <$> SDL.surfacePixels (fbSurface fb)
  let p = Ptr.plusPtr base
        $ 4
        * (fromIntegral y * fromIntegral screenWidth + fromIntegral x)
  liftIO $ St.poke p c
  SDL.unlockSurface $ fbSurface fb

blitTile :: V.Vector Word8 -> Addr -> (Word8, Word8) -> Framebuffer -> IO ()
blitTile v (Addr a) (bx, by) fb = do
  (ps :: [(Word8, Word8, Word8)]) <- mconcat <$> forM [0..7] \y -> do
    mconcat <$> forM [0..1] \xb -> do
      b <- case v V.!? fromIntegral (a + (y * 2 + xb)) of
        Just b -> pure b
        Nothing -> throwM . VideoError $ mconcat
          [ "tile address ", pretty $ Addr a
          , " out of bounds"
          ]
      pure
        [ (bx + fromIntegral xb, by + fromIntegral y, w8bits2 7 b)
        , (bx + fromIntegral xb + 1, by + fromIntegral y, w8bits2 5 b)
        , (bx + fromIntegral xb + 2, by + fromIntegral y, w8bits2 3 b)
        , (bx + fromIntegral xb + 3, by + fromIntegral y, w8bits2 1 b)
        ]
  forM_ ps \(x, y, p) -> do
    unless (p == 0) do
      blitPixel
        (case p of
           0x01 -> 0xff0000ff
           0x02 -> 0x990000ff
           0x03 -> 0x330000ff
           _else -> error "unreachable"
        )
        (x, y) fb

data VideoAddrRange
  = VideoAddrVRAM Addr
  | VideoAddrStatus Addr
  deriving Show

videoAddrRange :: Addr -> Maybe VideoAddrRange
videoAddrRange a = if
  | a >= 0x8000 && a < 0xa000 -> Just . VideoAddrVRAM $ a - 0x8000
  | a >= 0xff40 && a <= 0xff4b -> Just . VideoAddrStatus $ a - 0xff40
  | otherwise -> Nothing

data RenderState
  = RenderOAMSearch
  | RenderPixelTransfer
  | RenderHBlank
  | RenderVBlank
  deriving Show

data VideoState = VideoState
  { vstFb :: Framebuffer
  , vstVRAM :: V.Vector Word8
  , vstScx :: Word8
  , vstScy :: Word8
  , vstLy :: Word8
  , vstLx :: Word8
  , vstRenderState :: RenderState
  , vstTick :: Word16
  }

compVideo :: Framebuffer -> Component
compVideo framebuffer = Component
  { compState = VideoState
    { vstFb = framebuffer
    , vstVRAM = V.replicate (8 * 1024) 0
    , vstScx = 0
    , vstScy = 0
    , vstLy = 0
    , vstLx = 0
    , vstRenderState = RenderOAMSearch
    , vstTick = 0
    }
  , compMatches = isJust  . videoAddrRange
  , compUpdate = \olds t -> {-# SCC "ComponentVideoUpdate" #-} do
      let tick = vstTick olds + fromIntegral t
      let s = olds { vstTick = tick }
      case vstRenderState s of
        RenderOAMSearch
          | tick == 40 -> pure s
            { vstRenderState = RenderPixelTransfer
            }
          | otherwise -> pure s
        RenderPixelTransfer
          | tick == 200 -> pure s
            { vstRenderState = RenderHBlank
            }
          | otherwise -> pure s
        RenderHBlank
          | tick == 456 -> do
              let ly = vstLy olds + 1
              if ly == 144
                then
                pure s { vstTick = 0, vstLy = ly, vstRenderState = RenderVBlank }
                else
                pure s { vstTick = 0, vstLy = ly, vstRenderState = RenderOAMSearch }
          | otherwise -> pure s
        RenderVBlank
          | tick == 456 -> do
              let ly = vstLy olds + 1
              if ly == 153
                then do
                -- log "vblank"
                -- logTilemap $ vstVRAM s
                -- forM_ (zip [0..] [48,52,45,111,112,32,114,44,105,109,109] :: [(Int, Int)]) \(idx, b) -> do
                --   blitTile (vstVRAM s)
                --     (fromIntegral $ b * 16)
                --     (fromIntegral $ idx * 8, 0)
                --     $ vstFb s
                -- forM_ ([0..10] :: [Int]) \x -> do
                --   forM_ ([0..10] :: [Int]) \y -> do
                --     let i = y * 10 + x
                --     blitTile (vstVRAM s)
                --       (0x200 + fromIntegral i * 16)
                --       (fromIntegral $ x * 8, fromIntegral $ y * 8)
                --       $ vstFb s
                pure s { vstTick = 0, vstLy = 0, vstRenderState = RenderOAMSearch }
                else
                pure s { vstTick = 0, vstLy = ly }
          | otherwise -> pure s
  , compWrite = \s a v -> {-# SCC "ComponentVideoWrite" #-} case videoAddrRange a of
      Nothing -> throwM $ VideoError $ mconcat
        [ "write address out of bounds for video system: "
        , pretty a
        ]
      Just (VideoAddrVRAM off) -> do
        -- log $ "write to vram offset: " <> pretty off
        pure s
          { vstVRAM =
            V.modify (\ms -> MV.write ms (fromIntegral $ unAddr off) v) $ vstVRAM s
          }
      Just (VideoAddrStatus off) -> case off of
        0x0 -> pure s
        0x1 -> pure s
        0x2 -> pure s { vstScx = v }
        0x3 -> pure s { vstScy = v }
        0x4 -> pure s
        0x5 -> pure s
        0x6 -> pure s
        0x7 -> pure s
        0x8 -> pure s
        0x9 -> pure s
        0xa -> pure s
        0xb -> pure s
        _ -> pure s
  , compRead = \s a -> {-# SCC "ComponentVideoRead" #-} case videoAddrRange a of
      Nothing -> throwM $ VideoError $ mconcat
        [ "read address out of bounds for video system: "
        , pretty a
        ]
      Just (VideoAddrStatus off) -> case off of
        0x0 -> pure 0x00
        0x1 -> pure 0x00
        0x2 -> pure $ vstScx s
        0x3 -> pure $ vstScy s
        0x4 -> pure $ vstLy s
        0x5 -> pure 0x00
        0x6 -> pure 0x00
        0x7 -> pure 0x00
        0x8 -> pure 0x00
        0x9 -> pure 0x00
        0xa -> pure 0x00
        0xb -> pure 0x00
        _ -> pure 0x00
      Just (VideoAddrVRAM off) -> do
        case vstVRAM s V.!? fromIntegral (unAddr off) of
          Nothing -> error "unreachable"
          Just v -> pure v
  }

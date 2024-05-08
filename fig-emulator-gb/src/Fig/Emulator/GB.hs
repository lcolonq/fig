module Fig.Emulator.GB where

import Prelude (error)
import Fig.Prelude

import System.IO (withFile, IOMode (WriteMode))

import Control.Lens ((.=), use)
import Control.Monad (when)
import Control.Monad.State.Strict (StateT(..))

import qualified Data.Vector as V
import qualified Data.ByteString as BS

import qualified SDL

import Fig.Emulator.GB.CPU
import Fig.Emulator.GB.CPU.Instruction
import Fig.Emulator.GB.Bus (Bus(..), Addr(..))
import Fig.Emulator.GB.Component.RAM
import Fig.Emulator.GB.Component.ROM
import Fig.Emulator.GB.Component.Video
import Fig.Emulator.GB.Component.Joystick
import Fig.Emulator.GB.Component.Serial
import Fig.Emulator.GB.Component.Interrupt (compInterrupt)

cpuDMG :: Maybe Handle -> ByteString -> Framebuffer -> CPU
cpuDMG serial rom fb = CPU
  { _lastPC = 0x0
  , _lastIns = Nop
  , _running = True
  , _regs = initialRegs
  , _bus = Bus
    [ compROM rom
    , compWRAM 0xc000 $ 8 * 1024
    , compVideo fb
    , compJoystick
    , compSerial serial
    , compInterrupt
    , compWRAM 0xff80 0x7e -- HRAM
    ]
  }

testRun :: Maybe FilePath -> ByteString -> IO ()
testRun serialOut rom = do
  SDL.initializeAll
  window <- SDL.createWindow "taking" SDL.defaultWindow
  fb <- initializeFramebuffer
  let withSerial f = case serialOut of
        Nothing -> f Nothing
        Just p -> withFile p WriteMode $ f . Just
  liftIO $ withSerial \hserial -> do
    let cpu = cpuDMG hserial rom fb
    let
      loop :: Int -> Emulating ()
      loop cycle = do
        -- events <- SDL.pollEvents
        -- forM_ events \ev ->
        --   case SDL.eventPayload ev of
        --     SDL.QuitEvent -> running .= False
        --     _else -> pure ()
        pc <- use $ regs . regPC
        ins <- decode
        when (pc == 0x2817) do
          log $ mconcat
            [ pretty $ Addr pc
            , ": ", tshow ins
            ]
        step ins
        when (rem cycle 1000000 == 0) do
          log "1 million"
        -- when (rem cycle 70224 == 0) do
        --   ws <- SDL.getWindowSurface window
        --   SDL.surfaceFillRect ws Nothing $ SDL.V4 0x00 0x00 0x00 0xff
        --   void $
        --     SDL.surfaceBlitScaled
        --     (fbSurface fb)
        --     Nothing
        --     ws
        --     (Just $ SDL.Rectangle
        --      (SDL.P $ SDL.V2 0 0)
        --      (SDL.V2
        --       (fromIntegral screenWidth * 8)
        --       (fromIntegral screenHeight * 8)))
        --   SDL.updateWindowSurface window
        r <- use running
        when r . loop $ cycle + 1
    void $ flip (runStateT . runEmulating) cpu do
      loop 0

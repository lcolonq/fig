module Fig.Bus.Binary (main) where

import Fig.Prelude

import Control.Monad (when)
import Control.Concurrent.MVar as MVar

import Data.Word (Word8, Word32)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import qualified Data.List as List
import Data.ByteString (hPut, hGet)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IORef

import Fig.Utils.Net

newtype EventType = EventType ByteString
  deriving (Show, Eq, Ord)

newtype BusState = BusState
  { subscriptions :: Map EventType [Handle]
  }

subscribe :: EventType -> Handle -> BusState -> BusState
subscribe ev h bs = bs
  { subscriptions = Map.insertWith (<>) ev [h] bs.subscriptions
  }

unsubscribe :: EventType -> Handle -> BusState -> BusState
unsubscribe ev h bs = bs
  { subscriptions = Map.update (Just . List.delete h) ev bs.subscriptions
  }

intFromLEBytes :: [Word8] -> Int
intFromLEBytes [] = 0
intFromLEBytes (x:xs) = shiftL (intFromLEBytes xs) 8 .|. fromIntegral x

readLengthPrefixed :: Handle -> IO (Maybe ByteString)
readLengthPrefixed h = do
  n <- hGet h 4
  log $ "parsing: " <> tshow n
  case intFromLEBytes (BS.unpack n) of
    0 -> pure Nothing
    len -> do
      log $ "reading: " <> tshow len
      x <- hGet h len
      pure $ Just x

readEvent :: Handle -> IO (Maybe EventType)
readEvent h = do
  mb <- readLengthPrefixed h
  pure $ EventType <$> mb

writeLengthPrefixed :: Handle -> ByteString -> IO ()
writeLengthPrefixed h d = do
  let l :: Word32 = fromIntegral $ BS.length d
  let bytes =
        [ fromIntegral $ l .&. 0xff
        , fromIntegral $ shiftR l 8 .&. 0xff
        , fromIntegral $ shiftR l 16 .&. 0xff
        , fromIntegral $ shiftR l 24 .&. 0xff
        ]
  hPut h $ BS.pack bytes <> d 

writeEvent :: Handle -> EventType -> IO ()
writeEvent h (EventType d) = writeLengthPrefixed h d

publish :: EventType -> ByteString -> BusState -> IO ()
publish ev d bs =
  case Map.lookup ev bs.subscriptions of
    Nothing -> pure ()
    Just hs -> forM_ hs \h -> do
      writeEvent h ev
      writeLengthPrefixed h d

main :: (Maybe Text, Text) -> IO ()
main bind = do
  st <- MVar.newMVar $ BusState { subscriptions = Map.empty }
  server bind do
    subs <- IORef.newIORef ([] :: [EventType])
    pure \h peer ->
      ( do
          let
            go = do
              c <- hGet h 1
              when (BS.length c == 1) do
                case BS.head c of
                  115 -> readEvent h >>= \case
                    Just ev -> do
                      log $ tshow peer <> " subscribing to: " <> tshow ev
                      IORef.modifyIORef' subs (ev:)
                      MVar.modifyMVar_ st (pure . subscribe ev h)
                      go
                    _ -> log "malformed subscription"
                  112 -> (,) <$> readEvent h <*> readLengthPrefixed h >>= \case
                    (Just ev, Just d) -> do
                      log $ tshow peer <> " publishing to: " <> tshow ev
                      publish ev d =<< MVar.readMVar st
                      go
                    _ -> log "malformed publish"
                  _ -> log "unknown"
          go
      , do
          ss <- IORef.readIORef subs
          MVar.modifyMVar_ st \bs -> pure $ foldr (`unsubscribe` h) bs ss
      )

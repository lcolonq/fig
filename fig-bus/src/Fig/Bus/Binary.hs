module Fig.Bus.Binary (main) where

import Fig.Prelude

import Control.Concurrent.MVar as MVar

import qualified Data.List as List
import Data.ByteString (hGet)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IORef

import Fig.Utils.Net
import Fig.Bus.Binary.Utils

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
          log $ "Connected: " <> tshow peer
          let
            go = do
              c <- hGet h 1
              when (BS.length c == 1) do
                case BS.head c of
                  115 -> readEvent h >>= \case
                    Just ev@(EventType e) -> do
                      log $ tshow peer <> " subscribing to: " <> tshow e
                      IORef.modifyIORef' subs (ev:)
                      MVar.modifyMVar_ st (pure . subscribe ev h)
                      go
                    _else -> log "Malformed subscription"
                  112 -> (,) <$> readEvent h <*> readLengthPrefixed h >>= \case
                    (Just ev@(EventType e), Just d) -> do
                      log $ tshow peer <> " publishing to: " <> tshow e
                      publish ev d =<< MVar.readMVar st
                      go
                    _else -> log "Malformed publish"
                  w -> log $ "Unknown command code: " <> tshow w
          go
      , do
          log $ "Disconnected: " <> tshow peer
          ss <- IORef.readIORef subs
          MVar.modifyMVar_ st \bs -> pure $ foldr (`unsubscribe` h) bs ss
      )

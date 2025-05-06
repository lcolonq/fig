module Fig.Bus.Binary.Client (Commands(..), busClient) where

import Fig.Prelude

import System.Exit (exitFailure)

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Async

import Data.ByteString (hPut)

import Fig.Utils.Net
import Fig.Bus.Binary.Utils

data Commands m = Commands
  { subscribe :: !(ByteString -> m ())
  , publish :: !(ByteString -> ByteString -> m ())
  }

newtype FigBusClientException = FigBusClientException Text
  deriving (Show, Eq, Ord)
instance Exception FigBusClientException

busClient :: forall m.
  (MonadIO m, MonadThrow m, MonadMask m) =>
  (Text, Text) ->
  (Commands IO -> IO ()) ->
  (Commands IO -> ByteString -> ByteString -> IO ()) ->
  IO () ->
  m ()
busClient loc@(host, port) onConn onData onQuit = catchFailure . client loc $ pure \h ->
  let
    cmds = Commands
      { subscribe = \ev -> do
          hPut h "s"
          writeLengthPrefixed h ev
      , publish = \ev d -> do
          hPut h "p"
          writeLengthPrefixed h ev
          writeLengthPrefixed h d
      }
  in
    ( do
        liftIO $ Async.concurrently_ (onConn cmds) do
          forever do
            (,) <$> readLengthPrefixed h <*> readLengthPrefixed h >>= \case
              (Just ev, Just d) -> liftIO $ onData cmds ev d
              _else -> throwM . FigBusClientException $ "Server sent malformed data"
    , liftIO onQuit
    )
  where
    catchFailure body = catch body \(e :: IOException) -> do
      log $ "Failed to connect to bus at " <> host <> ":" <> port <> ": " <> tshow e
      liftIO exitFailure

_testClient :: IO ()
_testClient = busClient ("localhost", "32050")
  (\cmds -> do
      cmds.subscribe "foo"
      forever do
        Conc.threadDelay 1000000
        cmds.publish "bar" "42"
  )
  (\_cmds ev d -> log $ "Received: " <> tshow (ev, d))
  (pure ())

{-# Language QuasiQuotes #-}

module Fig.Bus.Binary.Client (Commands(..), busClient) where

import Fig.Prelude

import System.Exit (exitFailure)

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Async

import Data.ByteString (hPut, hGet)

import Fig.Utils.Net
import Fig.Bus.Binary

data Commands m = Commands
  { subscribe :: EventType -> m ()
  , publish :: EventType -> ByteString -> m ()
  }

newtype FigBusClientException = FigBusClientException Text
  deriving (Show, Eq, Ord)
instance Exception FigBusClientException

busClient :: forall m.
  (MonadIO m, MonadThrow m, MonadMask m) =>
  (Text, Text) ->
  (Commands IO -> IO ()) ->
  (Commands IO -> SExpr -> IO ()) ->
  IO () ->
  m ()
busClient loc@(host, port) onConn onData onQuit = catchFailure . client loc $ pure \h ->
  let
    cmds = Commands
      { subscribe = \(EventType ev) -> do
          hPut h "s"
          writeLengthPrefixed h ev
      , publish = \(EventType ev) d -> do
          hPut h "p"
          writeLengthPrefixed h ev
          writeLengthPrefixed h d
      }
  in
    ( do
        liftIO $ Async.concurrently_ (onConn cmds) do
          forever do
            
            line <- throwLeft id . decodeUtf8' =<< liftIO (hGetLine h)
            case parseSExpr line of
              Nothing -> throwM . FigBusClientException $ "Server sent malformed s-expression: " <> line
              Just x -> liftIO $ onData cmds x
    , liftIO onQuit
    )
  where
    catchFailure body = catch body \(e :: IOException) -> do
      log $ "Failed to connect to bus at " <> host <> ":" <> port <> ": " <> tshow e
      liftIO exitFailure

_testClient :: IO ()
_testClient = busClient ("localhost", "32050")
  (\cmds -> do
      cmds.subscribe [sexp|foo|]
      forever do
        Conc.threadDelay 1000000
        cmds.publish [sexp|bar|] [[sexp|42|]]
  )
  (\_cmds d -> log $ "Received: " <> pretty d)
  (pure ())

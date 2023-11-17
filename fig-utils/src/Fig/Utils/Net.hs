module Fig.Utils.Net
  ( server
  , client
  ) where

import Fig.Prelude

import System.IO (IOMode(..), BufferMode(..), hClose, hSetBuffering)

import qualified Control.Concurrent as Conc

import qualified Network.Socket as Sock

newtype FigNetException = FigNetException Text
  deriving (Show, Eq, Ord)
instance Exception FigNetException

resolveAddr :: forall m.
  (MonadIO m, MonadThrow m) =>
  (Maybe Text, Text) ->
  Bool ->
  m Sock.AddrInfo
resolveAddr (host, port) serv = do
  maddr <- liftIO $ headMay <$> Sock.getAddrInfo
    (Just $ Sock.defaultHints
     { Sock.addrFlags = [Sock.AI_PASSIVE | serv]
     , Sock.addrSocketType = Sock.Stream
     }
    )
    (unpack <$> host)
    (Just $ unpack port)
  maybe (throwM $ FigNetException "Failed to resolve address") pure maddr

server :: forall m.
  (MonadIO m, MonadThrow m, MonadMask m) =>
  (Maybe Text, Text) ->
  m (Handle -> Sock.SockAddr -> (IO (), IO ())) ->
  m ()
server loc onConn = do
  addr <- resolveAddr loc True
  bracket (liftIO $ Sock.openSocket addr) (liftIO . Sock.close) \sock -> do
    liftIO $ Sock.setSocketOption sock Sock.ReuseAddr 1
    liftIO $ Sock.withFdSocket sock Sock.setCloseOnExecIfNeeded
    liftIO $ Sock.bind sock $ Sock.addrAddress addr
    liftIO $ Sock.listen sock 4096
    log $ "Listening on " <> tshow (Sock.addrAddress addr)
    forever do
      let toHandle = bracketOnError (liftIO $ Sock.accept sock) (liftIO . Sock.close . fst) \(conn, peer) ->
            liftIO $ (,peer) <$> Sock.socketToHandle conn ReadWriteMode
      bracketOnError toHandle (liftIO . hClose . fst) \(hdl, peer) -> do
        liftIO $ log $ "Client " <> tshow peer <> " connected"
        liftIO $ hSetBuffering hdl LineBuffering
        (handler, cleanup) <- ($ peer) . ($ hdl) <$> onConn
        liftIO $ Conc.forkFinally handler \res -> do
          case res of
            Right _ -> log $ "Client " <> tshow peer <> " disconnected"
            Left err -> log $ "Client " <> tshow peer <> " disconnected: " <> tshow err
          cleanup
          hClose hdl

client :: forall m.
  (MonadIO m, MonadThrow m, MonadMask m) =>
  (Text, Text) ->
  m (Handle -> (m (), m ())) ->
  m ()
client loc onConn = do
  addr <- resolveAddr (first Just loc) False
  let openConnectHandle = do
        bracketOnError (liftIO $ Sock.openSocket addr) (liftIO . Sock.close) \sock -> do
          liftIO . Sock.connect sock $ Sock.addrAddress addr
          hdl <- liftIO $ Sock.socketToHandle sock ReadWriteMode
          (handler, cleanup) <- ($ hdl) <$> onConn
          pure (hdl, handler, cleanup)
  bracket openConnectHandle
    ( \(hdl, _, cleanup) -> do
        cleanup 
        liftIO $ hClose hdl
    )
    ( \(_, handler, _) -> do
        handler
    )

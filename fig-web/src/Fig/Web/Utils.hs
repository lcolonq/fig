{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Fig.Web.Utils
  ( FigWebException(..)
  , loadConfig
  , Config(..)
  , websocket
  , module Network.HTTP.Types.Status
  ) where

import Fig.Prelude

import Network.HTTP.Types.Status
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets as WS

import qualified Web.Scotty as Sc

import qualified Toml

newtype FigWebException = FigWebException Text
  deriving (Show, Eq, Ord)
instance Exception FigWebException

data Config = Config
  { port :: !Int
  , clientId :: !Text
  , authToken :: !Text
  , dbHost :: !Text
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  port <- Toml.int "port" Toml..= (\a -> a.port)
  clientId <- Toml.text "client_id" Toml..= (\a -> a.clientId)
  authToken <- Toml.text "auth_token" Toml..= (\a -> a.authToken)
  dbHost <- Toml.text "db_host" Toml..= (\a -> a.dbHost)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigWebException $ tshow err
  Right config -> pure config

websocket :: ByteString -> (WS.Connection -> IO ()) -> Sc.ScottyM ()
websocket pat h = Sc.middleware $ Wai.WS.websocketsOr WS.defaultConnectionOptions handler
  where
    handler pending = if WS.requestPath (WS.pendingRequest pending) == pat
      then WS.acceptRequest pending >>= h
      else WS.rejectRequest pending ""

{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Fig.Monitor.Twitch.Utils
  ( FigMonitorTwitchException(..)
  , loadConfig
  , RequestConfig(..)
  , Config(..)
  , authedRequest
  , authedRequestJSON
  , Authed
  , runAuthed
  , userIsLiveScrape
  ) where

import Fig.Prelude

import Control.Monad.Reader (ReaderT, runReaderT)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Map.Strict as Map

import qualified Toml

import qualified Data.Aeson as Aeson

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP

newtype FigMonitorTwitchException = FigMonitorTwitchException Text
  deriving (Show, Eq, Ord)
instance Exception FigMonitorTwitchException

data Config = Config
  { clientId :: Text
  , userToken :: Text
  , userLogin :: Text
  , monitor :: [Text]
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  clientId <- Toml.text "client_id" Toml..= (\a -> a.clientId)
  userToken <- Toml.text "user_token" Toml..= (\a -> a.userToken)
  -- userIds <- Toml.arrayOf Toml._Text "user_ids" Toml..= (\a -> a.userIds)
  userLogin <- Toml.text "user_login" Toml..= (\a -> a.userLogin)
  monitor <- Toml.arrayOf Toml._Text "monitor" Toml..= (\a -> a.monitor)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigMonitorTwitchException $ tshow err
  Right config -> pure config

data RequestConfig = RequestConfig
  { config :: Config
  , manager :: HTTP.Manager
  }

newtype Authed a = Authed { unAuthed :: ReaderT RequestConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader RequestConfig, MonadIO, MonadThrow)

authedRequest :: Text -> Text -> BS.Lazy.ByteString -> Authed BS.Lazy.ByteString
authedRequest method url body = do
  rc <- ask
  initialRequest <- liftIO . HTTP.parseRequest $ unpack url
  let request = initialRequest
        { method = encodeUtf8 method
        , requestBody = RequestBodyLBS body
        , requestHeaders = 
            [ ("Authorization", encodeUtf8 $ "Bearer " <> rc.config.userToken)
            , ("Client-Id", encodeUtf8 rc.config.clientId)
            , ("Content-Type", "application/json")
            ]
        }
  response <- liftIO $ HTTP.httpLbs request rc.manager
  pure $ HTTP.responseBody response

authedRequestJSON :: (Aeson.ToJSON a, Aeson.FromJSON b) => Text -> Text -> a -> Authed b
authedRequestJSON method url val = do
  resp <- authedRequest method url $ Aeson.encode val
  case Aeson.eitherDecode resp of
    Left err -> do
      throwM . FigMonitorTwitchException $ tshow err
    Right res -> pure res

runAuthed :: Config -> Authed a -> IO a
runAuthed config body = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  runReaderT body.unAuthed RequestConfig{..}

userIsLiveScrape :: Text -> Authed Bool
userIsLiveScrape user = do
  rc <- ask
  request <- liftIO . HTTP.parseRequest $ mconcat
    [ "https://twitch.tv/"
    , unpack user
    ]
  response <- liftIO $ HTTP.httpLbs request rc.manager
  let res = BS.isInfixOf "\"isLiveBroadcast\":true" . BS.Lazy.toStrict $ HTTP.responseBody response
  log $ mconcat
    [ user
    , " is "
    , if res then "" else "not "
    , "live"
    ]
  pure res

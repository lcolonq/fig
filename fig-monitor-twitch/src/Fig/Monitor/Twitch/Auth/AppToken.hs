module Fig.Monitor.Twitch.Auth.AppToken
  ( Authed
  , RequestConfig(..)
  , authedRequest, authedRequestJSON
  , runAuthed
  ) where

import Fig.Prelude

import qualified Data.ByteString.Lazy as BS.Lazy

import Control.Monad.Reader (ReaderT, runReaderT)

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Client.MultipartFormData as HTTP

import Fig.Monitor.Twitch.Utils

data RequestConfig = RequestConfig
  { config :: Config
  , manager :: HTTP.Manager
  , appToken :: Text
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
            [ ("Authorization", encodeUtf8 $ "Bearer " <> rc.appToken)
            , ("Client-Id", encodeUtf8 rc.config.clientId)
            , ("Content-Type", "application/json")
            ]
        }
  response <- liftIO $ HTTP.httpLbs request rc.manager
  pure $ HTTP.responseBody response

authedRequestJSON :: (Aeson.ToJSON a, Aeson.FromJSON b) => Text -> Text -> Maybe a -> Authed b
authedRequestJSON method url val = do
  resp <- authedRequest method url $ maybe "" Aeson.encode val
  case Aeson.eitherDecode resp of
    Left err -> do
      throwM . FigMonitorTwitchException $ tshow err
    Right res -> pure res

getAppToken :: HTTP.Manager -> Config -> IO Text
getAppToken manager config = do
  initialRequest <- HTTP.parseRequest "https://id.twitch.tv/oauth2/token"
  let preRequest = initialRequest
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/json")]
        }
  request <- HTTP.formDataBody
    [ partBS "client_id" $ encodeUtf8 config.clientId
    , partBS "client_secret" $ encodeUtf8 config.clientSecret
    , partBS "grant_type" "client_credentials"
    ]
    preRequest
  response <- liftIO $ HTTP.httpLbs request manager
  case Aeson.eitherDecode $ HTTP.responseBody response of
    Left err -> throwM . FigMonitorTwitchException $ tshow err
    Right v -> case Aeson.parseMaybe (.: "access_token") v of
      Nothing -> throwM $ FigMonitorTwitchException "failed to obtain access token"
      Just t -> pure t

runAuthed :: Config -> Authed a -> IO a
runAuthed config body = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  appToken <- getAppToken manager config
  log $ "got app token! " <> appToken
  runReaderT body.unAuthed RequestConfig{..}

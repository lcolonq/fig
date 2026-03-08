module Fig.Monitor.Twitch.Auth.UserToken
  ( Authed
  , RequestConfig(..)
  , authedRequest, authedRequestJSON
  , runAuthed
  ) where

import Fig.Prelude

import qualified Data.ByteString.Lazy as BS.Lazy

import Control.Monad.Reader (ReaderT, runReaderT)

import qualified Data.Aeson as Aeson

import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTP

import Fig.Monitor.Twitch.Utils

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

authedRequestJSON :: (Aeson.ToJSON a, Aeson.FromJSON b) => Text -> Text -> Maybe a -> Authed b
authedRequestJSON method url val = do
  resp <- authedRequest method url $ maybe "" Aeson.encode val
  case Aeson.eitherDecode resp of
    Left err -> do
      throwM . FigMonitorTwitchException $ tshow err
    Right res -> pure res

runAuthed :: Config -> Authed a -> IO a
runAuthed config body = do
  manager <- HTTP.newManager HTTP.tlsManagerSettings
  runReaderT body.unAuthed RequestConfig{..}

{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Fig.Monitor.Twitch.Utils
  ( FigMonitorTwitchException(..)
  , loadConfig
  , Config(..)
  ) where

import Fig.Prelude

import qualified Toml

newtype FigMonitorTwitchException = FigMonitorTwitchException Text
  deriving (Show, Eq, Ord)
instance Exception FigMonitorTwitchException

data Config = Config
  { clientId :: Text
  , clientSecret :: Text
  , userToken :: Text
  , userLogin :: Text
  , monitor :: [Text]
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  clientId <- Toml.text "client_id" Toml..= (\a -> a.clientId)
  clientSecret <- Toml.text "client_secret" Toml..= (\a -> a.clientSecret)
  userToken <- Toml.text "user_token" Toml..= (\a -> a.userToken)
  userLogin <- Toml.text "user_login" Toml..= (\a -> a.userLogin)
  monitor <- Toml.arrayOf Toml._Text "monitor" Toml..= (\a -> a.monitor)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigMonitorTwitchException $ tshow err
  Right config -> pure config

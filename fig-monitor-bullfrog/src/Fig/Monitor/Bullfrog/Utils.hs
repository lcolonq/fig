{-# Language ApplicativeDo #-}

module Fig.Monitor.Bullfrog.Utils
  ( FigMonitorBullfrogException(..)
  , Config(..)
  , loadConfig
  ) where

import Fig.Prelude

import qualified Toml

newtype FigMonitorBullfrogException = FigMonitorBullfrogException Text
  deriving (Show, Eq, Ord)
instance Exception FigMonitorBullfrogException

newtype Config = Config
  { authToken :: Text
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  authToken <- Toml.text "auth_token" Toml..= (\a -> a.authToken)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigMonitorBullfrogException $ tshow err
  Right config -> pure config

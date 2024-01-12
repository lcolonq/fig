{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Fig.Frontend.Utils
  ( FigFrontendException(..)
  , loadConfig
  , Config(..)
  , module Network.HTTP.Types.Status
  ) where

import Fig.Prelude

import Network.HTTP.Types.Status

import qualified Toml

newtype FigFrontendException = FigFrontendException Text
  deriving (Show, Eq, Ord)
instance Exception FigFrontendException

data Config = Config
  { port :: Int
  , assetPath :: FilePath
  , clientId :: Text
  , authToken :: Text
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  port <- Toml.int "port" Toml..= (\a -> a.port)
  assetPath <- Toml.string "asset_path" Toml..= (\a -> a.assetPath)
  clientId <- Toml.text "client_id" Toml..= (\a -> a.clientId)
  authToken <- Toml.text "auth_token" Toml..= (\a -> a.authToken)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigFrontendException $ tshow err
  Right config -> pure config

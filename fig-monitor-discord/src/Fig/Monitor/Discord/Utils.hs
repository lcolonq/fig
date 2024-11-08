{-# Language ApplicativeDo #-}

module Fig.Monitor.Discord.Utils
  ( FigMonitorDiscordException(..)
  , Config(..)
  , loadConfig
  ) where

import Fig.Prelude

import qualified Toml

newtype FigMonitorDiscordException = FigMonitorDiscordException Text
  deriving (Show, Eq, Ord)
instance Exception FigMonitorDiscordException

data Config = Config
  { authToken :: !Text
  , guildIds :: ![Integer]
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  authToken <- Toml.text "auth_token" Toml..= (\a -> a.authToken)
  guildIds <- Toml.arrayOf Toml._Integer "guild_ids" Toml..= (\a -> a.guildIds)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigMonitorDiscordException $ tshow err
  Right config -> pure config

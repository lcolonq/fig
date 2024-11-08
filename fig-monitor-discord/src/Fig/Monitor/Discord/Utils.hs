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
  , guildId :: !Integer
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  authToken <- Toml.text "auth_token" Toml..= (\a -> a.authToken)
  guildId <- Toml.integer "guild_id" Toml..= (\a -> a.guildId)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigMonitorDiscordException $ tshow err
  Right config -> pure config

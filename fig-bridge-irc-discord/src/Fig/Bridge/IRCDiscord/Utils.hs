{-# Language ApplicativeDo #-}

module Fig.Bridge.IRCDiscord.Utils
  ( FigBridgeIRCDiscordException(..)
  , Config(..)
  , loadConfig
  ) where

import Fig.Prelude

import qualified Toml

newtype FigBridgeIRCDiscordException = FigBridgeIRCDiscordException Text
  deriving (Show, Eq, Ord)
instance Exception FigBridgeIRCDiscordException

newtype Config = Config
  { mapping :: [(Int, Text)]
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  mapping <- Toml.list (Toml.pair (Toml.int "discord") (Toml.text "irc")) "mapping" Toml..= (\a -> a.mapping)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigBridgeIRCDiscordException $ tshow err
  Right config -> pure config

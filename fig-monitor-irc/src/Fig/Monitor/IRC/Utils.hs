{-# Language ApplicativeDo #-}

module Fig.Monitor.IRC.Utils
  ( FigMonitorIRCException(..)
  , Config(..)
  , loadConfig
  ) where

import Fig.Prelude

import qualified Toml

newtype FigMonitorIRCException = FigMonitorIRCException Text
  deriving (Show, Eq, Ord)
instance Exception FigMonitorIRCException

data Config = Config
  { host :: Text
  , port :: Int
  , nick :: Text
  , sendchannel :: Text
  , channels :: [Text]
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  host <- Toml.text "host" Toml..= (\a -> a.host)
  port <- Toml.int "port" Toml..= (\a -> a.port)
  nick <- Toml.text "nick" Toml..= (\a -> a.nick)
  sendchannel <- Toml.text "sendchannel" Toml..= (\a -> a.sendchannel)
  channels <- Toml.arrayOf Toml._Text "channels" Toml..= (\a -> a.channels)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigMonitorIRCException $ tshow err
  Right config -> pure config

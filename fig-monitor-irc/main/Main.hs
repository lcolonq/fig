module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Monitor.IRC
import Fig.Monitor.IRC.Utils

data Opts = Opts
  { busHost :: Text
  , busPort :: Text
  , config :: FilePath
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "bus-host" <> metavar "HOST" <> help "Address of message bus" <> value "localhost")
  <*> strOption (long "bus-port" <> metavar "PORT" <> help "Message bus port" <> showDefault <> value "32050")
  <*> strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "fig-monitor-irc.toml")

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-monitor-discord - monitor IRC chat events"
    )
  cfg <- loadConfig opts.config
  ircBot cfg (opts.busHost, opts.busPort)

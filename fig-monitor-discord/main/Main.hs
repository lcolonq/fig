module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Monitor.Discord
import Fig.Monitor.Discord.Utils

data Opts = Opts
  { busHost :: Text
  , busPort :: Text
  , config :: FilePath
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "bus-host" <> metavar "HOST" <> help "Address of message bus" <> value "localhost")
  <*> strOption (long "bus-port" <> metavar "PORT" <> help "Message bus port" <> showDefault <> value "32050")
  <*> strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "fig-monitor-discord.toml")

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-monitor-discord - monitor Discord chat events"
    )
  cfg <- loadConfig opts.config
  discordBot cfg (opts.busHost, opts.busPort)

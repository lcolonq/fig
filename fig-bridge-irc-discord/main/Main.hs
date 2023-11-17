module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Bridge.IRCDiscord

data Opts = Opts
  { busHost :: Text
  , busPort :: Text
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "bus-host" <> metavar "HOST" <> help "Address of message bus" <> value "localhost")
  <*> strOption (long "bus-port" <> metavar "PORT" <> help "Message bus port" <> showDefault <> value "32050")

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-bridge-irc-discord - bridge between IRC and Discord"
    )
  bridge (opts.busHost, opts.busPort)

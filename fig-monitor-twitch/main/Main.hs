module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Monitor.Twitch
import Fig.Monitor.Twitch.Utils

data Command
  = Monitor
  | Chatbot
  | LiveChecker
  | RedirectServer !Bool
  | Validate

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
  [ command "monitor" $ info (pure Monitor) (progDesc "Launch the Twitch monitor")
  , command "chatbot" $ info (pure Chatbot) (progDesc "Launch the Twitch chatbot")
  , command "live-checker" $ info (pure LiveChecker) (progDesc "Launch the Twitch live status checker")
  , command "user-token-server" $ info (pure $ RedirectServer True) (progDesc "Launch a web server to handle authentication redirects")
  , command "user-token-server-read-only" $ info (pure $ RedirectServer False) (progDesc "Launch a web server to handle authentication redirects")
  , command "validate-endpoint" $ info (pure Validate) (progDesc "Test Twitch authentication")
  ]
data Opts = Opts
  { busHost :: !Text
  , busPort :: !Text
  , config :: !FilePath
  , command :: !Command
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "bus-host" <> metavar "HOST" <> help "Address of message bus" <> value "localhost")
  <*> strOption (long "bus-port" <> metavar "PORT" <> help "Message bus port" <> showDefault <> value "32050")
  <*> strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "fig-monitor-twitch.toml")
  <*> parseCommand

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-monitor-twitch - monitor Twitch.tv stream events"
    )
  cfg <- loadConfig opts.config
  case opts.command of
    Monitor -> twitchEventClient cfg (opts.busHost, opts.busPort)
    Chatbot -> twitchChatClient cfg (opts.busHost, opts.busPort)
    LiveChecker -> twitchChannelLiveMonitor cfg (opts.busHost, opts.busPort)
    RedirectServer rw -> userTokenRedirectServer cfg rw
    Validate -> twitchEndpointTest cfg

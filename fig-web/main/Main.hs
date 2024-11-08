{-# Language ApplicativeDo #-}

module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Web
import Fig.Web.Utils

data Opts = Opts
  { busHost :: Text
  , busPort :: Text
  , config :: FilePath
  }

parseOpts :: Parser Opts
parseOpts = do
  busHost <- strOption (long "bus-host" <> metavar "HOST" <> help "Address of message bus" <> value "localhost")
  busPort <- strOption (long "bus-port" <> metavar "PORT" <> help "Message bus port" <> showDefault <> value "32050")
  config <- strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "fig-web.toml")
  pure Opts{..}

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-web - public-facing web applications"
    )
  cfg <- loadConfig opts.config
  server cfg (opts.busHost, opts.busPort)

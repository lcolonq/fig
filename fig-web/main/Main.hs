{-# Language ApplicativeDo #-}

module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Web.Utils
import qualified Fig.Web.Public as Public
import qualified Fig.Web.Secure as Secure

data Command
  = Public
  | Secure

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
  [ command "public" $ info (pure Public) (progDesc "Launch the public web server")
  , command "secure" $ info (pure Secure) (progDesc "Launch the private web server (intended to be run behind authentication proxy)")
  ]

data Opts = Opts
  { busHost :: !Text
  , busPort :: !Text
  , config :: !FilePath
  , cmd :: !Command
  }

parseOpts :: Parser Opts
parseOpts = do
  busHost <- strOption (long "bus-host" <> metavar "HOST" <> help "Address of message bus" <> value "localhost")
  busPort <- strOption (long "bus-port" <> metavar "PORT" <> help "Message bus port" <> showDefault <> value "32050")
  config <- strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "fig-web.toml")
  cmd <- parseCommand
  pure Opts{..}

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-web - public-facing web applications"
    )
  cfg <- loadConfig opts.config
  case opts.cmd of
    Public -> Public.server cfg (opts.busHost, opts.busPort)
    Secure -> Secure.server cfg (opts.busHost, opts.busPort)

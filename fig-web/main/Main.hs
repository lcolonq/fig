{-# Language ApplicativeDo #-}

module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Web.Types
import Fig.Web.Utils
import qualified Fig.Utils.FFI as FFI
import qualified Fig.Web.Public as Public
import qualified Fig.Web.Secure as Secure

parsePublicOptions :: Parser PublicOptions
parsePublicOptions = do
  pure PublicOptions{}

parseSecureOptions :: Parser SecureOptions
parseSecureOptions = do
  simAuth <- switch (long "sim-auth" <> help "Simulate authentication instead of actually requiring authentication proxy headers")
  pure SecureOptions{..}

data Command
  = Public PublicOptions
  | Secure SecureOptions
  | TestFFI

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
  [ command "public" $ info (Public <$> parsePublicOptions) (progDesc "Launch the public web server")
  , command "secure" $ info (Secure <$> parseSecureOptions) (progDesc "Launch the private web server (intended to be run behind authentication proxy)")
  , command "testffi" $ info (pure TestFFI) (progDesc "Test the FFI")
  ]

data Opts = Opts
  { busHost :: !Text
  , busPort :: !Text
  , port :: !(Maybe Int)
  , config :: !FilePath
  , cmd :: !Command
  }

parseOpts :: Parser Opts
parseOpts = do
  busHost <- strOption (long "bus-host" <> metavar "HOST" <> help "Address of message bus" <> value "localhost")
  busPort <- strOption (long "bus-port" <> metavar "PORT" <> help "Message bus port" <> showDefault <> value "32050")
  port <- optional $ option auto (long "port" <> metavar "PORT" <> help "Web server port")
  config <- strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "fig-web.toml")
  cmd <- parseCommand
  pure Opts{..}

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> Options.Applicative.header "fig-web - web backends"
    )
  icfg <- loadConfig opts.config
  let cfg = case opts.port of
        Nothing -> icfg
        Just p -> icfg { Fig.Web.Utils.port = p }
  case opts.cmd of
    Public o -> Public.server o cfg (opts.busHost, opts.busPort)
    Secure o -> Secure.server o cfg (opts.busHost, opts.busPort)
    TestFFI -> do
      log "testing FFI"
      res <- FFI.checkAnswer "(lambda (d e) (equal? d e))" "hello computer" "hello computer"
      log $ "result: " <> tshow res
      inpa <- FFI.genInputAnswer "(lambda (x) (cons x x))" "hello computer"
      log $ "input/answer: " <> tshow inpa

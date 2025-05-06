module Main where

import Fig.Prelude

import Options.Applicative

import qualified Fig.Bus.SExp as SExp
import qualified Fig.Bus.Binary as Binary

data Command = SExp | Binary

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
  [ command "sexp" $ info (pure SExp) (progDesc "Launch the s-expression bus")
  , command "binary" $ info (pure Binary) (progDesc "Launch the binary bus")
  ]

data Opts = Opts
  { host :: !Text
  , port :: !Text
  , cmd :: !Command
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "host" <> metavar "HOST" <> help "Interface to bind" <> value "localhost")
  <*> strOption (long "port" <> metavar "PORT" <> help "Port to bind" <> showDefault <> value "32050")
  <*> parseCommand

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-bus - a pub/sub message bus"
    )
  case opts.cmd of
    SExp -> SExp.main (Just opts.host, opts.port)
    Binary -> Binary.main (Just opts.host, opts.port)

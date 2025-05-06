module Main where

import Fig.Prelude

import Options.Applicative

import qualified Fig.Bus
import qualified Fig.Bus.Binary

data Opts = Opts
  { host :: Text
  , port :: Text
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "host" <> metavar "HOST" <> help "Interface to bind" <> value "localhost")
  <*> strOption (long "port" <> metavar "PORT" <> help "Port to bind" <> showDefault <> value "32050")

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-bus - a pub/sub message bus"
    )
  -- Fig.Bus.main (Just opts.host, opts.port)
  Fig.Bus.Binary.main (Just opts.host, opts.port)

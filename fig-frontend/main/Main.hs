module Main where

import Fig.Prelude

import Options.Applicative

import Fig.Frontend
import Fig.Frontend.Utils

newtype Opts = Opts
  { config :: FilePath
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "config" <> metavar "PATH" <> help "Path to config file" <> showDefault <> value "fig-frontend.toml")

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> header "fig-frontend - public-facing web applications"
    )
  cfg <- loadConfig opts.config
  server cfg

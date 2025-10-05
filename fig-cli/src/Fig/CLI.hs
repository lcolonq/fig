{-# Language ApplicativeDo #-}

module Fig.CLI where

import Fig.Prelude

import Options.Applicative

import Fig.Utils.SExpr

newtype Opts = Opts
  { sexpr :: Text
  }

parseOpts :: Parser Opts
parseOpts = do
  sexpr <- strArgument (metavar "SEXPR" <> help "S-expression to parse")
  pure Opts{..}

main :: IO ()
main = do
  opts <- execParser $ info (parseOpts <**> helper)
    ( fullDesc
    <> Options.Applicative.header "fig-cli - assorted tools"
    )
  let sexp = parseSExpr opts.sexpr
  log $ tshow (sexp, pretty <$> sexp)

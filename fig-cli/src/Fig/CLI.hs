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
  let x = parseSExpr opts.sexpr
  log $ tshow (opts.sexpr, x, pretty <$> x, parseSExpr . pretty =<< x)
  log $ tshow $ (pretty <$> x) == Just opts.sexpr

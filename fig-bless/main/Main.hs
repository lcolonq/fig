{-# Language ApplicativeDo, ImplicitParams #-}

module Main where

import Fig.Prelude

import Options.Applicative

import Control.Exception.Safe (Handler(..), catches)

import Data.Text.IO (putStrLn)

import Fig.Bless
import qualified Fig.Bless.Syntax as Syn

data EvalOptions = EvalOptions
  { src :: Text
  , fuel :: Maybe Integer
  }

newtype Command
  = Eval EvalOptions

newtype Options = Options
  { cmd :: Command
  }

parseEvalOptions :: Parser EvalOptions
parseEvalOptions = do
  fuel <- optional $ option auto (long "fuel" <> short 'f' <> metavar "N" <> help "Maximum number of terms to run")
  src <- unwords <$> some (argument str (metavar "TERM..."))
  pure EvalOptions{..}

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
  [ command "eval" $ info (Eval <$> parseEvalOptions) (progDesc "Evaluate a Bless program")
  ]

parseOptions :: Parser Options
parseOptions = do
  cmd <- parseCommand
  pure Options{..}

main :: IO ()
main = do
  opts <- execParser $ info (parseOptions <**> helper)
    ( fullDesc
    <> header "fig-bless - tools for the Bless language"
    )
  catches
    ( case opts.cmd of
        Eval eo -> do
          prog <- parse "<input>" (programF spanning <* eof) eo.src
          let ?term = Nothing
          let vm = initialize eo.fuel (Dictionary mempty) arithmetic
          vm' <- runProgram (pure . unSpanning) prog vm
          let
            stack :: [ValueF Spanning (Fix (ValueF Spanning))]
            stack = vm'.stack
          forM_ stack $ putStrLn . pretty
    )
    [ Handler \(e :: Syn.ParseError) -> hPutStrLn stderr $ pretty e
    , Handler \(e :: RuntimeError Spanning) -> hPutStrLn stderr $ pretty e
    ]

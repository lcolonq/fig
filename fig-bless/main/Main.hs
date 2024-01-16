{-# Language ApplicativeDo, ImplicitParams #-}

module Main where

import Fig.Prelude

import Options.Applicative

import Control.Exception.Safe (Handler(..), catches)

import Data.Text.IO (putStrLn)
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString.Lazy as B.L

import qualified Data.Aeson as Aeson

import Fig.Bless
import qualified Fig.Bless.Syntax as Syn

data EvalOptions = EvalOptions
  { src :: Text
  , fuel :: Maybe Integer
  }

parseEvalOptions :: Parser EvalOptions
parseEvalOptions = do
  fuel <- optional $ option auto (long "fuel" <> short 'f' <> metavar "N" <> help "Maximum number of terms to run")
  src <- unwords <$> some (argument str (metavar "TERM..."))
  pure EvalOptions{..}

newtype TypeOptions = TypeOptions
  { src :: Text
  }

parseTypeOptions :: Parser TypeOptions
parseTypeOptions = do
  src <- unwords <$> some (argument str (metavar "TERM..."))
  pure TypeOptions{..}

data DictionaryOptions = DictionaryOptions
  { path :: FilePath
  , entrypoint :: Word
  , fuel :: Maybe Integer
  }

parseDictionaryOptions :: Parser DictionaryOptions
parseDictionaryOptions = do
  fuel <- optional $ option auto (long "fuel" <> short 'f' <> metavar "N" <> help "Maximum number of terms to run")
  entrypoint <- fmap (fromMaybe "main") . optional $ strOption (long "entrypoint" <> short 'e' <> metavar "WORD" <> help "Word to evaluate")
  path <- argument str (metavar "PATH")
  pure DictionaryOptions{..}

data Command
  = Eval EvalOptions
  | Type TypeOptions
  | Dict DictionaryOptions

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
  [ command "eval" $ info (Eval <$> parseEvalOptions) (progDesc "Evaluate a Bless program")
  , command "type" $ info (Type <$> parseTypeOptions) (progDesc "Check the type of a Bless program")
  , command "dictionary" $ info (Dict <$> parseDictionaryOptions) (progDesc "Load and run a Bless dictionary")
  ]

data Options = Options
  { cmd :: Command
  , json :: Bool
  }

parseOptions :: Parser Options
parseOptions = do
  json <- flag False True (long "json" <> short 'j' <> help "Write output as JSON")
  cmd <- parseCommand
  pure Options{..}

writeError :: (MonadIO m, Pretty e, Aeson.ToJSON e) => Options -> e -> m ()
writeError o e
  | o.json = liftIO . putStrLn . decodeUtf8 . B.L.toStrict $ Aeson.encode e
  | otherwise = liftIO . hPutStrLn stderr $ pretty e

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
          let ext = pure . unSpanning
          _ty <- typeOfProgram (initializeEnv ext builtins) prog
          let vm = initialize eo.fuel (Dictionary mempty) builtins
          vm' <- runProgram ext prog vm
          let
            stack :: [ValueF Spanning (Fix (ValueF Spanning))]
            stack = vm'.stack
          forM_ stack $ putStrLn . pretty
        Type to -> do
          prog <- parse "<input>" (programF spanning <* eof) to.src
          let ?term = Nothing
          let ext = pure . unSpanning
          ty <- typeOfProgram (initializeEnv ext builtins) prog
          putStrLn $ pretty ty
        Dict o -> do
          src <- T.IO.readFile o.path
          dict <- parse "<input>" (dictionaryF spanning <* eof) src
          let ?term = Nothing
          let ext = pure . unSpanning
          _env <- checkDictionary (initializeEnv ext builtins) dict
          let vm = initialize o.fuel dict builtins
          vm' <- runWord ext o.entrypoint vm
          let
            stack :: [ValueF Spanning (Fix (ValueF Spanning))]
            stack = vm'.stack
          forM_ stack $ putStrLn . pretty
    )
    [ Handler \(e :: Syn.ParseError) -> hPutStrLn stderr $ pretty e
    , Handler \(e :: RuntimeError Spanning) -> hPutStrLn stderr $ pretty e
    , Handler \(e :: TypeError Spanning) -> hPutStrLn stderr $ pretty e
    ]

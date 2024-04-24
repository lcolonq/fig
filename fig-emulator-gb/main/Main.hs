{-# Language ApplicativeDo #-}

module Main where

import Fig.Prelude

import Options.Applicative

import Control.Exception.Safe (Handler(..), catches)

import qualified Data.ByteString as BS

import Fig.Emulator.GB
import Fig.Emulator.GB.Test.Instr

data RunOptions = RunOptions
  { romPath :: !FilePath
  , serialOut :: !FilePath
  } deriving Show

parseRunOptions :: Parser RunOptions
parseRunOptions = do
  romPath <- argument str (metavar "PATH")
  serialOut <- strOption (long "serial" <> metavar "PATH" <> help "Path to write link cable serial output")
  pure RunOptions{..}

newtype InstrTestOptions = InstrTestOptions
  { testcasesPath :: FilePath
  } deriving Show

parseInstrTestOptions :: Parser InstrTestOptions
parseInstrTestOptions = do
  testcasesPath <- argument str (metavar "PATH")
  pure InstrTestOptions{..}

data Command
  = CommandRun RunOptions
  | CommandInstrTest InstrTestOptions
  deriving Show

parseOptions :: Parser Command
parseOptions = subparser $ mconcat
  [ command "run" $ info (CommandRun <$> parseRunOptions) (progDesc "Emulate a ROM file")
  , command "instr-test" $ info (CommandInstrTest <$> parseInstrTestOptions) (progDesc "Run CPU testcases")
  ]

main :: IO ()
main = do
  cmd <- execParser $ info (parseOptions <**> helper)
    ( fullDesc
    <> header "fig-emulator-gb - Game Boy emulator"
    )
  case cmd of
    CommandRun opts -> do
      rom <- BS.readFile $ romPath opts
      testRun (serialOut opts) rom
    CommandInstrTest opts -> catches
      ( do
          tcs <- readTestcases $ testcasesPath opts
          forM_ tcs runTestcase
      )
      [ Handler \(e :: InstrTestError) -> liftIO . hPutStrLn stderr $ pretty e
      ]

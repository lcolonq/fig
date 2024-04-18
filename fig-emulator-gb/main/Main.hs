{-# Language ApplicativeDo #-}

module Main where

import Fig.Prelude

import Options.Applicative

import qualified Data.ByteString as BS

import Fig.Emulator.GB

newtype Options = Options
  { romPath :: FilePath
  } deriving Show

parseOptions :: Parser Options
parseOptions = do
  romPath <- argument str (metavar "PATH")
  pure Options{..}

main :: IO ()
main = do
  opts <- execParser $ info (parseOptions <**> helper)
    ( fullDesc
    <> header "fig-emulator-gb - Game Boy emulator"
    )
  rom <- BS.readFile $ romPath opts
  testRun rom

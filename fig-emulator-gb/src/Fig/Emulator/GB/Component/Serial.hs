module Fig.Emulator.GB.Component.Serial where

import Fig.Prelude

import GHC.IO.Handle (hPutChar)

import Data.Char (chr)

import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.Bus

newtype SerialError = SerialError Text
  deriving Show
instance Exception SerialError
instance Pretty SerialError where
  pretty (SerialError b) = mconcat
    [ "serial error: "
    , b
    ]

compSerial :: Maybe Handle -> Component
compSerial mh = Component
  { compState = ()
  , compMatches = (== 0xff01)
  , compUpdate = \s _ -> pure s
  , compWrite = \s _ v -> do
      log $ mconcat
        [ "wrote serial byte: ", tshow $ chr $ fromIntegral v
        ]
      case mh of
        Nothing -> pure ()
        Just h -> liftIO . hPutChar h . chr $ fromIntegral v
      pure s
  , compRead = \_ _ -> pure 0x00
  }

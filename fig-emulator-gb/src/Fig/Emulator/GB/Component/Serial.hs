module Fig.Emulator.GB.Component.Serial where

import Fig.Prelude
import Prelude (fromIntegral)

import Data.Char (chr)

import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.Bus

newtype SerialError = SerialError Text
  deriving Show
instance Exception SerialError
instance Pretty SerialError where
  pretty (SerialError b) = mconcat
    [ "joystick error: "
    , b
    ]

compSerial :: (MonadIO m, MonadThrow m) => Component m
compSerial = Component
  { compState = ()
  , compMatches = (== 0xff01)
  , compUpdate = \s _ -> pure s
  , compWrite = \s _ v -> do
      log $ mconcat
        [ "wrote serial byte: ", tshow $ chr $ fromIntegral v
        ]
      pure s
  , compRead = \_ _ -> pure 0x00
  }

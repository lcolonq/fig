module Fig.Emulator.GB.Component.Interrupt where

import Fig.Prelude

import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.Bus

newtype InterruptError = InterruptError Text
  deriving Show
instance Exception InterruptError
instance Pretty InterruptError where
  pretty (InterruptError b) = mconcat
    [ "interrupt error: "
    , b
    ]

compInterrupt :: (MonadIO m, MonadThrow m) => Component m
compInterrupt = Component
  { compState = ()
  , compMatches = \a -> a == 0xff0f || a == 0xffff
  , compUpdate = \s _ -> pure s
  , compWrite = \s (Addr a) v -> do
      case a of
        0xff0f -> do
          -- log $ "set IF:" <> show8 v
          pure ()
        0xffff -> do
          -- log $ "set IE:" <> show8 v
          pure ()
        _ -> throwM . InterruptError $ "write to invalid address: " <> pretty (Addr a)
      pure s
  , compRead = \_ _ -> pure 0x00
  }

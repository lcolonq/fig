module Fig.Emulator.GB.Component.Video where

import Fig.Prelude

import Fig.Emulator.GB.Bus

newtype VideoError = VideoError Text
  deriving Show
instance Exception VideoError
instance Pretty VideoError where
  pretty (VideoError b) = mconcat
    [ "video error: "
    , b
    ]

compLCD :: (MonadIO m, MonadThrow m) => Component m
compLCD = Component
  { compState = ()
  , compMatches = \a -> a >= 0xff40 && a <= 0xff4b
  , compUpdate = pure
  , compWrite = \s _ _ -> pure s
  , compRead = \_ (Addr a) -> do
      let off = a - 0xff40
      case off of
        0x0 -> pure 0x00
        0x1 -> pure 0x00
        0x2 -> pure 0x00
        0x3 -> pure 0x00
        0x4 -> pure 0x00
        0x5 -> pure 0x00
        0x6 -> pure 0x00
        0x7 -> pure 0x00
        0x8 -> pure 0x00
        0x9 -> pure 0x00
        0xa -> pure 0x00
        0xb -> pure 0x00
        _ -> throwM $ VideoError $ mconcat
          [ "address out of bounds for LCD: "
          , pretty $ Addr a
          ]
  }

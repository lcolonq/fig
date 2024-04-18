module Fig.Emulator.GB.Component.ROM
  ( compROM
  ) where

import Fig.Prelude
import Prelude (fromIntegral)

import qualified Data.Vector as V
import qualified Data.ByteString as BS

import Fig.Emulator.GB.Bus

newtype ROMError = ROMError Text
  deriving Show
instance Exception ROMError
instance Pretty ROMError where
  pretty (ROMError b) = mconcat
    [ "internal ROM error: "
    , b
    ]

-- | Initialize base ROM (no mapper) from a ByteString
compROM :: (MonadIO m, MonadThrow m) => ByteString -> Component m
compROM bs = Component
  { compState = V.fromList $ BS.unpack bs
  , compMatches = \a ->
      a >= start && a < end
  , compUpdate = \s _ -> pure s
  , compWrite = \s _ad _v ->
      pure s
      -- throwM . ROMError $ mconcat
      -- [ "tried to write to ROM at ", pretty ad
      -- ]
  , compRead = \s ad -> do
      let offset = fromIntegral . unAddr $ ad - start
      case s V.!? offset of
        Nothing -> throwM . ROMError $ mconcat
          [ "address ", pretty ad, " out of bounds"
          ]
        Just v -> pure v
  }
  where
    start = 0x0000
    -- end = 0x4000
    end = 0x8000

module Fig.Emulator.GB.Component.RAM
  ( compWRAM
  ) where

import Fig.Prelude
import Prelude (fromIntegral)

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word8)

import Fig.Emulator.GB.Bus

newtype RAMError = RAMError Text
  deriving Show
instance Exception RAMError
instance Pretty RAMError where
  pretty (RAMError b) = mconcat
    [ "internal RAM error: "
    , b
    ]

compWRAM :: (MonadIO m, MonadThrow m) => Addr -> Int -> Component m
compWRAM start size = Component
  { compState = V.replicate size 0 :: V.Vector Word8
  , compMatches = \a ->
      a >= start && a < end
  , compUpdate = pure
  , compWrite = \s ad v -> do
      let offset = fromIntegral . unAddr $ ad - start
      pure $ V.modify (\ms -> MV.write ms offset v) s
  , compRead = \s ad -> do
      let offset = fromIntegral . unAddr $ ad - start
      case s V.!? offset of
        Nothing -> throwM . RAMError $ mconcat
          [ "address ", pretty ad, " out of bounds"
          ]
        Just v -> pure v
  }
  where
    end = start + Addr (fromIntegral size)

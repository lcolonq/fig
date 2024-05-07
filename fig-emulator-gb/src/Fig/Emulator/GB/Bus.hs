module Fig.Emulator.GB.Bus
  ( Addr(..)
  , Component(..)
  , Bus(..)
  , update
  , write
  , read
  ) where

import Fig.Prelude

import Numeric (showHex)

import qualified Data.List as List
import Data.Word (Word16, Word8)

newtype Addr = Addr { unAddr :: Word16 }
  deriving (Show, Num, Eq, Ord)
instance Pretty Addr where
  pretty (Addr w) = "$" <> pack (showHex w "")

data Component m = forall (s :: Type). Component
  { compState :: s
  , compMatches :: Addr -> Bool
  , compUpdate :: s -> Int -> m s
  , compWrite :: s -> Addr -> Word8 -> m s
  , compRead :: s -> Addr -> m Word8
  }

newtype Bus m = Bus { busComponents :: [Component m] }

update :: forall m. MonadIO m => Int -> Bus m -> m (Bus m)
update t b = Bus <$> forM (busComponents b) \Component{..} -> do
  s <- compUpdate compState t
  pure Component { compState = s, ..}

write :: forall m. MonadIO m => Bus m -> Addr -> Word8 -> m (Bus m)
write b a v = Bus <$> forM (busComponents b) \c@Component{..} ->
  if compMatches a
  then do
    s <- compWrite compState a v
    pure Component { compState = s, ..}
  else pure c

read :: forall m. (MonadIO m, MonadThrow m) => Bus m -> Addr -> m (Maybe Word8)
read b a = case List.find (`compMatches` a) $ busComponents b of
  Nothing -> pure Nothing
  Just Component{..} -> Just <$> compRead compState a

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

data Component = forall (s :: Type). Component
  { compState :: !s
  , compMatches :: !(Addr -> Bool)
  , compUpdate :: !(s -> Word16 -> IO s)
  , compWrite :: !(s -> Addr -> Word8 -> IO s)
  , compRead :: !(s -> Addr -> IO Word8)
  }

newtype Bus = Bus { busComponents :: [Component] }

update :: Word16 -> Bus -> IO Bus
update t b = Bus <$> forM (busComponents b) \Component{..} -> do
  s <- compUpdate compState t
  pure Component { compState = s, ..}

write :: Bus -> Addr -> Word8 -> IO Bus
write b a v = Bus <$> forM (busComponents b) \c@Component{..} ->
  if compMatches a
  then do
    s <- compWrite compState a v
    pure Component { compState = s, ..}
  else pure c

read :: Bus -> Addr -> IO (Maybe Word8)
read b a = case List.find (`compMatches` a) $ busComponents b of
  Nothing -> pure Nothing
  Just Component{..} -> Just <$> compRead compState a

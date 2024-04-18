module Fig.Emulator.GB.Utils where

import Fig.Prelude

import Prelude (fromIntegral)

import qualified Text.Printf as Pr

import Data.Word (Word8, Word16)
import Data.Int (Int8)
import Data.Bits

show8 :: Word8 -> Text
show8 = pack . Pr.printf "%02X"

show16 :: Word8 -> Text
show16 = pack . Pr.printf "%04X"

w8w8 :: Word8 -> Word8 -> Word16
w8w8 high low = shiftL (fromIntegral high) 8 .|. fromIntegral low

w16hi :: Word16 -> Word8
w16hi v = fromIntegral $ shiftR v 8

w16lo :: Word16 -> Word8
w16lo v = fromIntegral $ v .&. 0xff

w8bit :: Int -> Word8 -> Bool
w8bit i v = shiftR v i .&. 0b1 == 1

w8bits2 :: Int -> Word8 -> Word8
w8bits2 i v = shiftR v (i - 1) .&. 0b11

w8bits3 :: Int -> Word8 -> Word8
w8bits3 i v = shiftR v (i - 2) .&. 0b111

w8bits4 :: Int -> Word8 -> Word8
w8bits4 i v = shiftR v (i - 3) .&. 0b1111

flagsw8 :: Bool -> Bool -> Bool -> Bool -> Word8
flagsw8 z n h c =
  shiftL (if z then 1 else 0) 7
  .|. shiftL (if n then 1 else 0) 6
  .|. shiftL (if h then 1 else 0) 5
  .|. shiftL (if c then 1 else 0) 4

zext :: Word8 -> Word16
zext = fromIntegral

sext :: Word8 -> Word16
sext x = fromIntegral y
  where
    y :: Int8
    y = fromIntegral x

trunc :: Word16 -> Word8
trunc = fromIntegral

addC :: Bool -> Word8 -> Word8 -> (Word8, Bool)
addC c x y = (trunc res, shiftR res 8 .&. 1 == 1)
  where
    res :: Word16
    res = sext x + sext y + if c then 1 else 0

addH :: Bool -> Word8 -> Word8 -> Bool
addH c x y = shiftR res 4 .&. 1 == 1
  where
    xlo = x .&. 0xf
    ylo = y .&. 0xf
    res :: Word8
    res = xlo + ylo + if c then 1 else 0

subH :: Word8 -> Word8 -> Bool
subH x y = w8bits4 3 x < w8bits4 3 y

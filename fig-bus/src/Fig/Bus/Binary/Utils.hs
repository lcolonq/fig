module Fig.Bus.Binary.Utils where

import Fig.Prelude

import Data.Word (Word8, Word32)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.ByteString (hPut, hGet)
import qualified Data.ByteString as BS

newtype EventType = EventType ByteString
  deriving (Show, Eq, Ord)

intFromLEBytes :: [Word8] -> Int
intFromLEBytes [] = 0
intFromLEBytes (x:xs) = shiftL (intFromLEBytes xs) 8 .|. fromIntegral x

readLengthPrefixed :: Handle -> IO (Maybe ByteString)
readLengthPrefixed h = do
  n <- hGet h 4
  case intFromLEBytes (BS.unpack n) of
    0 -> pure $ Just ""
    len -> do
      x <- hGet h len
      pure $ Just x

readEvent :: Handle -> IO (Maybe EventType)
readEvent h = do
  mb <- readLengthPrefixed h
  pure $ EventType <$> mb

writeLengthPrefixed :: Handle -> ByteString -> IO ()
writeLengthPrefixed h d = do
  let l :: Word32 = fromIntegral $ BS.length d
  let bytes =
        [ fromIntegral $ l .&. 0xff
        , fromIntegral $ shiftR l 8 .&. 0xff
        , fromIntegral $ shiftR l 16 .&. 0xff
        , fromIntegral $ shiftR l 24 .&. 0xff
        ]
  hPut h $ BS.pack bytes <> d 

writeEvent :: Handle -> EventType -> IO ()
writeEvent h (EventType d) = writeLengthPrefixed h d

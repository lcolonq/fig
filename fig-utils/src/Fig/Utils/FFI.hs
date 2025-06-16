module Fig.Utils.FFI where

import Fig.Prelude

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.String
import Foreign.Marshal.Alloc

foreign import ccall "check_answer" c_check_answer :: Ptr CString -> CString -> CString -> IO Int

checkAnswer :: MonadIO m => Text -> Text -> m (Either Text Bool)
checkAnswer tcode tanswer = liftIO $
  withCString (unpack tcode) $ \code ->
  withCString (unpack tanswer) $ \answer ->
  alloca $ \rerr -> do
  res <- c_check_answer rerr code answer
  err <- peek rerr
  if err == nullPtr
  then pure . Right $ res /= 0
  else do
    msg <- peekCString err
    pure . Left $ pack msg

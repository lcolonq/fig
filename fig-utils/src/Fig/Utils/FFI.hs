module Fig.Utils.FFI where

import Fig.Prelude

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.C.String
import Foreign.Marshal.Alloc

foreign import ccall "check_answer" c_check_answer :: Ptr CString -> CString -> CString -> CString -> IO Int

checkAnswer :: MonadIO m => Text -> Text -> Text -> m (Either Text Bool)
checkAnswer tcode tanswer texpected = liftIO $
  withCString (unpack tcode) $ \code ->
  withCString (unpack tanswer) $ \answer ->
  withCString (unpack texpected) $ \expected ->
  alloca $ \rerr -> do
  res <- c_check_answer rerr code answer expected
  err <- peek rerr
  if err == nullPtr
  then pure . Right $ res /= 0
  else do
    msg <- peekCString err
    free err
    pure . Left $ pack msg

foreign import ccall "gen_input_answer" c_gen_input_answer :: Ptr CString -> Ptr CString -> Ptr CString -> CString -> CString -> IO ()

genInputAnswer :: MonadIO m => Text -> Text -> m (Either Text (Text, Text))
genInputAnswer tcode ttwitchid = liftIO $ 
  withCString (unpack tcode) $ \code ->
  withCString (unpack ttwitchid) $ \twitchid ->
  alloca $ \rerr ->
  alloca $ \rinput ->
  alloca $ \ranswer -> do
  c_gen_input_answer rerr rinput ranswer code twitchid
  err <- peek rerr
  if err == nullPtr
  then do
    input <- peekCString =<< peek rinput
    answer <- peekCString =<< peek ranswer
    pure $ Right (pack input, pack answer)
  else do
    msg <- peekCString err
    free err
    pure . Left $ pack msg

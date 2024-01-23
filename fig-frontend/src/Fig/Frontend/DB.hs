module Fig.Frontend.DB where

import Control.Error.Util (hush)

import qualified Database.Redis as Redis

import Fig.Prelude

connect :: MonadIO m => m Redis.Connection
connect = liftIO $ Redis.checkedConnect Redis.defaultConnectInfo
  { Redis.connectHost = "shiro"
  }

get :: MonadIO m => Redis.Connection -> ByteString -> m (Maybe ByteString)
get c key = liftIO $ Redis.runRedis c do
  v <- Redis.get key
  pure $ join $ hush v

hvals :: MonadIO m => Redis.Connection -> ByteString -> m (Maybe [ByteString])
hvals c key = liftIO $ Redis.runRedis c do
  hush <$> Redis.hvals key

module Fig.Web.DB where

import Control.Error.Util (hush)

import qualified Database.Redis as Redis

import Fig.Prelude
import Fig.Web.Types
import Fig.Web.Utils

connect :: MonadIO m => Config -> m DB
connect cfg = liftIO $ DB <$> Redis.checkedConnect Redis.defaultConnectInfo
  { Redis.connectHost = unpack cfg.dbHost
  }

get :: MonadIO m => DB -> ByteString -> m (Maybe ByteString)
get (DB c) key = liftIO $ Redis.runRedis c do
  v <- Redis.get key
  pure . join $ hush v

incr :: MonadIO m => DB -> ByteString -> m ()
incr (DB c) key = liftIO $ Redis.runRedis c do
  void $ Redis.incr key

decr :: MonadIO m => DB -> ByteString -> m ()
decr (DB c) key = liftIO $ Redis.runRedis c do
  void $ Redis.decr key

hget :: MonadIO m => DB -> ByteString -> ByteString -> m (Maybe ByteString)
hget (DB c) key hkey = liftIO $ Redis.runRedis c do
  v <- Redis.hget key hkey
  pure . join $ hush v

hkeys :: MonadIO m => DB -> ByteString -> m (Maybe [ByteString])
hkeys (DB c) key = liftIO $ Redis.runRedis c do
  hush <$> Redis.hkeys key

hvals :: MonadIO m => DB -> ByteString -> m (Maybe [ByteString])
hvals (DB c) key = liftIO $ Redis.runRedis c do
  hush <$> Redis.hvals key

sadd :: MonadIO m => DB -> ByteString -> [ByteString] -> m ()
sadd (DB c) key skeys = liftIO $ Redis.runRedis c do
  _ <- Redis.sadd key skeys
  pure ()

srem :: MonadIO m => DB -> ByteString -> [ByteString] -> m ()
srem (DB c) key skeys = liftIO $ Redis.runRedis c do
  _ <- Redis.srem key skeys
  pure ()

smembers :: MonadIO m => DB -> ByteString -> m (Maybe [ByteString])
smembers (DB c) key = liftIO $ Redis.runRedis c do
  hush <$> Redis.smembers key

sismember :: MonadIO m => DB -> ByteString -> ByteString -> m Bool
sismember (DB c) key skey = liftIO $ Redis.runRedis c do
  Redis.sismember key skey >>= hush >>> \case
    Just x -> pure x
    Nothing -> pure False

lpop :: MonadIO m => DB -> ByteString -> m (Maybe ByteString)
lpop (DB c) key = liftIO $ Redis.runRedis c do
  join . hush <$> Redis.lpop key

rpush :: MonadIO m => DB -> ByteString -> ByteString -> m ()
rpush (DB c) key val = liftIO $ Redis.runRedis c do
  _ <- Redis.rpush key [val]
  pure ()

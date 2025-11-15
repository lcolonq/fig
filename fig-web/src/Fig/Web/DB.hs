module Fig.Web.DB where

import Control.Error.Util (hush)

import Data.Maybe (mapMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

del :: MonadIO m => DB -> [ByteString] -> m ()
del (DB c) keys = liftIO $ Redis.runRedis c do
  void $ Redis.del keys

incr :: MonadIO m => DB -> ByteString -> m ()
incr (DB c) key = liftIO $ Redis.runRedis c do
  void $ Redis.incr key

decr :: MonadIO m => DB -> ByteString -> m ()
decr (DB c) key = liftIO $ Redis.runRedis c do
  void $ Redis.decr key

hset :: MonadIO m => DB -> ByteString -> ByteString -> ByteString -> m ()
hset (DB c) key hkey val = liftIO $ Redis.runRedis c do
  void $ Redis.hset key hkey val

hget :: MonadIO m => DB -> ByteString -> ByteString -> m (Maybe ByteString)
hget (DB c) key hkey = liftIO $ Redis.runRedis c do
  v <- Redis.hget key hkey
  pure . join $ hush v

hmset :: MonadIO m => DB -> ByteString -> [(ByteString, ByteString)] -> m ()
hmset (DB c) key m = liftIO $ Redis.runRedis c do
  void $ Redis.hmset key m

hmget :: MonadIO m => DB -> ByteString -> [ByteString] -> m (Map ByteString ByteString)
hmget (DB c) key hk = liftIO $ Redis.runRedis c do
  Redis.hmget key hk >>= \case
    Left _ -> pure Map.empty
    Right vals -> do
      pure . Map.fromList . mapMaybe (\(a, mb) -> mb >>= \b -> Just (a, b)) $ zip hk vals 

hgetall :: MonadIO m => DB -> ByteString -> m (Map ByteString ByteString)
hgetall (DB c) key = liftIO $ Redis.runRedis c do
  Redis.hgetall key >>= \case
    Left _ -> pure Map.empty
    Right m -> pure $ Map.fromList m

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

lrange :: MonadIO m => DB -> ByteString -> Integer -> Integer -> m [ByteString]
lrange (DB c) key start end = liftIO $ Redis.runRedis c do
  fromMaybe [] . hush <$> Redis.lrange key start end

llen :: MonadIO m => DB -> ByteString -> m (Maybe Integer)
llen (DB c) key = liftIO $ Redis.runRedis c do
  hush <$> Redis.llen key

lindex :: MonadIO m => DB -> ByteString -> Integer -> m (Maybe ByteString)
lindex (DB c) key idx = liftIO $ Redis.runRedis c do
  join . hush <$> Redis.lindex key idx

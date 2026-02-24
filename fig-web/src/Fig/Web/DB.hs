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

run :: MonadIO m => DB -> Redis.Redis a -> m a
run (DB c) f = liftIO $ Redis.runRedis c f

get :: ByteString -> Redis.Redis (Maybe ByteString)
get key = do
  v <- Redis.get key
  pure . join $ hush v

del :: [ByteString] -> Redis.Redis ()
del keys = do
  void $ Redis.del keys

incr :: ByteString -> Redis.Redis ()
incr key = do
  void $ Redis.incr key

decr :: ByteString -> Redis.Redis ()
decr key = do
  void $ Redis.decr key

hset :: ByteString -> ByteString -> ByteString -> Redis.Redis ()
hset key hkey val = do
  void $ Redis.hset key hkey val

hget :: ByteString -> ByteString -> Redis.Redis (Maybe ByteString)
hget key hkey = do
  v <- Redis.hget key hkey
  pure . join $ hush v

hdel :: ByteString -> ByteString -> Redis.Redis ()
hdel key hkey = do
  void $ Redis.hdel key [hkey]

hmset :: ByteString -> [(ByteString, ByteString)] -> Redis.Redis ()
hmset key m = do
  void $ Redis.hmset key m

hmget :: ByteString -> [ByteString] -> Redis.Redis (Map ByteString ByteString)
hmget key hk = do
  Redis.hmget key hk >>= \case
    Left _ -> pure Map.empty
    Right vals -> do
      pure . Map.fromList . mapMaybe (\(a, mb) -> mb >>= \b -> Just (a, b)) $ zip hk vals 

hgetall :: ByteString -> Redis.Redis (Map ByteString ByteString)
hgetall key = do
  Redis.hgetall key >>= \case
    Left _ -> pure Map.empty
    Right m -> pure $ Map.fromList m

hkeys :: ByteString -> Redis.Redis (Maybe [ByteString])
hkeys key = do
  hush <$> Redis.hkeys key

hvals :: ByteString -> Redis.Redis (Maybe [ByteString])
hvals key = do
  hush <$> Redis.hvals key

sadd :: ByteString -> [ByteString] -> Redis.Redis ()
sadd key skeys = do
  void $ Redis.sadd key skeys

srem :: ByteString -> [ByteString] -> Redis.Redis ()
srem key skeys = do
  void $ Redis.srem key skeys

smembers :: ByteString -> Redis.Redis (Maybe [ByteString])
smembers key = do
  hush <$> Redis.smembers key

sismember :: ByteString -> ByteString -> Redis.Redis Bool
sismember key skey = do
  Redis.sismember key skey >>= hush >>> \case
    Just x -> pure x
    Nothing -> pure False

lpop :: ByteString -> Redis.Redis (Maybe ByteString)
lpop key = do
  join . hush <$> Redis.lpop key

rpush :: ByteString -> ByteString -> Redis.Redis ()
rpush key val = do
  void $ Redis.rpush key [val]

lrange :: ByteString -> Integer -> Integer -> Redis.Redis [ByteString]
lrange key start end = do
  fromMaybe [] . hush <$> Redis.lrange key start end

llen :: ByteString -> Redis.Redis (Maybe Integer)
llen key = do
  hush <$> Redis.llen key

lindex :: ByteString -> Integer -> Redis.Redis (Maybe ByteString)
lindex key idx = do
  join . hush <$> Redis.lindex key idx

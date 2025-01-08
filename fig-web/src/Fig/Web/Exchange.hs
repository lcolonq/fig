module Fig.Web.Exchange where

import Control.Error.Util (hush)

import qualified Database.Redis as Redis

import Data.Maybe (mapMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Text.Read as Text.R
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Fig.Prelude

adjustUserCurrency :: Text -> Text -> Integer -> Redis.Redis ()
adjustUserCurrency user cur amt = do
  let key = "currency:" <> encodeUtf8 user
  let ecur = encodeUtf8 cur
  mold <- hush <$> Redis.hget key ecur
  let old = case mold of
        Just (Just o) -> case Text.R.decimal $ decodeUtf8 o of
          Right (num, _) -> num
          _else -> 0
        _else -> 0
  void . Redis.hset key ecur . encodeUtf8 . tshow $ old + amt

data Order = Order
  { creator :: !Text
  , wantCur :: !Text
  , wantAmount :: !Integer
  , haveCur :: !Text
  , haveAmount :: !Integer
  } deriving Generic
instance Aeson.ToJSON Order
instance Aeson.FromJSON Order

createOrder :: MonadIO m => Redis.Connection -> Order -> m ByteString
createOrder c o = liftIO $ Redis.runRedis c do
  let bs = Aeson.encode o
  uuid <- liftIO UUID.nextRandom
  let key = BS.Lazy.toStrict $ UUID.toByteString uuid
  void $ Redis.hset "orders" key (BS.Lazy.toStrict bs)
  pure key

getOrders :: MonadIO m => Redis.Connection -> m [Order]
getOrders c = liftIO $ Redis.runRedis c do
  Redis.hvals "orders" >>= \case
    Left _ -> pure []
    Right orders -> pure $ mapMaybe (Aeson.decode' . BS.Lazy.fromStrict) orders

cancelOrder :: MonadIO m => Redis.Connection -> ByteString -> m ()
cancelOrder c key = liftIO $ Redis.runRedis c do
  void $ Redis.hdel "orders" [key]

satisfyOrder :: MonadIO m => Redis.Connection -> ByteString -> Text -> m ()
satisfyOrder c key buyer = liftIO $ Redis.runRedis c do
  Redis.hget "orders" key >>= \case
    Right (Just bs) -> case Aeson.decode' $ BS.Lazy.fromStrict bs of
      Nothing -> pure ()
      Just (order :: Order) -> do
        adjustUserCurrency buyer order.wantCur (-order.wantAmount)
        adjustUserCurrency order.creator order.wantCur order.wantAmount
        adjustUserCurrency buyer order.haveCur order.haveAmount
        adjustUserCurrency order.creator order.haveCur (-order.haveAmount)
    _else -> pure ()
        

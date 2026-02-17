module Fig.Web.Module.User
  ( public
  ) where

import Fig.Prelude

import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.Map.Strict as Map

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

getText :: MonadIO m => DB -> ByteString -> m (Maybe Text)
getText db key = do
  DB.get db key >>= \case
    Nothing -> pure Nothing
    Just v -> pure . eitherToMaybe $ decodeUtf8' v

getTextList :: MonadIO m => DB -> ByteString -> m [Text]
getTextList db key = do
  xs <- DB.lrange db key 0 (-1)
  pure $ mapMaybe (eitherToMaybe . decodeUtf8') xs

getTextValuedMap :: MonadIO m => DB -> ByteString -> m (Map.Map Text Text)
getTextValuedMap db key = do
  vs <- Map.toList <$> DB.hgetall db key
  pure . Map.fromList $ flip mapMaybe vs \(k, v) -> do
    tk <- eitherToMaybe $ decodeUtf8' k
    tv <- eitherToMaybe $ decodeUtf8' v
    Just (tk, tv)

getIntegerValuedMap :: MonadIO m => DB -> ByteString -> m (Map.Map Text Integer)
getIntegerValuedMap db key = do
  vs <- Map.toList <$> DB.hgetall db key
  pure . Map.fromList $ flip mapMaybe vs \(k, v) -> do
    tk <- eitherToMaybe $ decodeUtf8' k
    (iv, _) <- BS.C8.readInteger v
    Just (tk, iv)

public :: PublicModule
public a = do
  -- users
  onGet "/api/user/:name" do
    name <- Text.toLower <$> pathParam "name"
    DB.get a.db ("user:" <> encodeUtf8 name) >>= \case
      Nothing -> do
        status status404
        respondText "user not found"
      Just val -> respondText $ decodeUtf8 val
  onGet "/api/user/stats/:uid" do
    uid <- pathParam "uid"
    respondJSON =<< getIntegerValuedMap a.db ("user:stats:" <> uid)
  onGet "/api/user/talents/:uid" do
    uid <- pathParam "uid"
    respondJSON =<< getIntegerValuedMap a.db ("user:talents:" <> uid)
  onGet "/api/user/properties/:uid" do
    uid <- pathParam "uid"
    respondJSON =<< getTextValuedMap a.db ("user:properties:" <> uid)
  onGet "/api/user/badges/:uid" do
    uid <- pathParam "uid"
    respondJSON =<< getTextList a.db ("user:badges:" <> uid)
  -- badges
  onGet "/api/badge/:uuid" do
    uuid <- pathParam "uuid"
    respondJSON =<< getTextList a.db ("badge:" <> uuid)

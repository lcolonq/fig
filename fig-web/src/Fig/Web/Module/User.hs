module Fig.Web.Module.User
  ( public
  ) where

import Fig.Prelude

import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson

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

data UserInfo = UserInfo
  { stats :: Map.Map Text Integer
  , talents :: Map.Map Text Integer
  , properties :: Map.Map Text Text
  , badges :: [Text]
  } deriving Generic
instance Aeson.ToJSON UserInfo 

getUserInfo :: MonadIO m => DB -> ByteString -> m UserInfo
getUserInfo db uid = do
  stats <- getIntegerValuedMap db $ "user:stats:" <> uid
  talents <- getIntegerValuedMap db $ "user:talents:" <> uid
  properties <- getTextValuedMap db $ "user:properties:" <> uid
  badges <- getTextList db $ "user:badges:" <> uid
  pure UserInfo{..}

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
  onGet "/api/user-id/:name" do
    name <- pathParam "name"
    getText a.db ("user-id:" <> encodeUtf8 (Text.toLower name)) >>= \case
      Nothing -> do
        status status404
        respondText "username not found"
      Just val -> respondText val
  onGet "/api/user/info/:uid" do -- get everything bundled together
    uid <- pathParam "uid"
    info <- getUserInfo a.db uid
    case Map.lookup "name" info.properties of
      Just _ -> respondJSON info
      Nothing -> do
        status status404
        respondText "no such user"
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
  onGet "/api/user/avatar/:uid.png" do
    uuidpng <- pathParam "uid.png"
    case Text.stripSuffix ".png" uuidpng of
      Nothing -> do
        status status400
        respondText "malformed user avatar path"
      Just uid -> DB.get a.db ("user:avatar:" <> encodeUtf8 uid) >>= \case
        Nothing -> do
          addHeader "Content-Type" "image/png"
          respondBytes "\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\STX\NUL\NUL\NUL\STX\b\ACK\NUL\NUL\NULr\182\r$\NUL\NUL\NUL\SOHsRGB\NUL\174\206\FS\233\NUL\NUL\NUL\SUBIDAT\b\153cd``\248\207T)\206\192\192T)\254\159\129\129\225?\NUL\RS\188\EOT#\137b%\ACK\NUL\NUL\NUL\NULIEND\174B`\130"
        Just img -> do
          addHeader "Content-Type" "image/png"
          respondBytes img
  -- badges
  onGet "/api/badge/:uuid" do
    uuid <- pathParam "uuid"
    respondJSON =<< getTextList a.db ("badge:" <> uuid)

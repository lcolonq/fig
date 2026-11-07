module Fig.Web.Module.Gif
  ( public
  ) where

import Fig.Prelude

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson

import qualified Database.Redis as Redis

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Utils.DB as DB

queryMatches :: Text -> Text -> Bool
queryMatches query tag = tag_in_query || query_in_tag
  where
    tlower = Text.toLower tag
    qlower = Text.toLower query
    tag_in_query = Text.isInfixOf tlower qlower
    qwords = Text.words qlower
    query_in_tag = any (`Text.isInfixOf` tlower) qwords

data Gif = Gif
  { name :: Text
  , creator_id :: Text
  , creator_name :: Maybe Text
  , timestamp :: Integer
  , tags :: [Text]
  } deriving Generic
instance Aeson.ToJSON Gif 

getGif :: ByteString -> Redis.Redis (Maybe Gif)
getGif ident = do
  meta <- DB.hgetall ("gif:metadata:" <> ident)
  case Map.lookup "creator" meta of
    Nothing -> pure Nothing
    Just cid -> do
      let creator_id = decodeUtf8 cid
      creator_name <- fmap decodeUtf8 <$> DB.hget ("user:properties:" <> cid) "name"
      pure do
        let name = decodeUtf8 ident
        timestamp <- readMaybe . Text.unpack . decodeUtf8 =<< Map.lookup "timestamp" meta
        tags <- Text.splitOn "\t".  decodeUtf8 <$> Map.lookup "tags" meta
        Just Gif{..} 

public :: PublicModule
public a = do
  onGet "/api/gif/search" do
    query :: Text <- queryParam "q"
    tags <- fmap decodeUtf8 . fromMaybe [] <$> DB.run a.db (DB.smembers "gif:tags")
    let matching = filter (queryMatches query) tags
    gifs <- DB.run a.db do
      ids <- DB.sunion $ ("gif:tags:"<>) . encodeUtf8 <$> matching
      mgifs <- mapM getGif ids
      pure $ catMaybes mgifs
    respondJSON gifs
  onGet "/gif/:uuid.gif" do
    uuidgif <- pathParam "uuid.gif"
    case Text.stripSuffix ".gif" uuidgif of
      Nothing -> do
        status status400
        respondText "malformed gif path"
      Just uuid -> DB.run a.db (DB.hget "gif:images" $ encodeUtf8 uuid) >>= \case
        Nothing -> do
          status status404
          respondText "gif does not exist"
        Just image -> do
          addHeader "Content-Type" "image/gif"
          addHeader "Cache-Control" "public, no-transform"
          respondBytes image

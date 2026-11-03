module Fig.Web.Module.Gif
  ( public
  ) where

import Fig.Prelude

import qualified Data.Text as Text

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Utils.DB as DB

public :: PublicModule
public a = do
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
          respondBytes image

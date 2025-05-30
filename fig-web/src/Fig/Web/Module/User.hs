module Fig.Web.Module.User
  ( public
  ) where

import Fig.Prelude

import qualified Data.Text as Text

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/user/:name" do
    name <- Text.toLower <$> pathParam "name"
    DB.get a.db ("user:" <> encodeUtf8 name) >>= \case
      Nothing -> do
        status status404
        respondText "user not found"
      Just val -> respondText $ decodeUtf8 val

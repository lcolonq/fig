module Fig.Web.Module.Bells
  ( public
  ) where

import Fig.Prelude

import Fig.Utils.SExpr
import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/songs" do
    DB.hvals a.db "songnames" >>= \case
      Nothing -> do
        status status404
        respondText "no sounds found :("
      Just songs -> respondText . pretty . SExprList @Void $ SExprString . decodeUtf8 <$> songs
  onGet "/api/song/:hash" do
    hash <- pathParam "hash"
    DB.hget a.db "songnotes" hash >>= \case
      Nothing -> do
        status status404
        respondText "song not found"
      Just val -> respondText $ decodeUtf8 val

module Fig.Web.Module.TCG
  ( public
  ) where

import Fig.Prelude

import qualified Data.Text as Text

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/tcg/card/:uuid.png" do
    uuidpng <- pathParam "uuid.png"
    case Text.stripSuffix ".png" uuidpng of
      Nothing -> do
        status status400
        respondText "malformed card path"
      Just uuid -> DB.hget a.db "tcg:cards" (encodeUtf8 uuid) >>= \case
        Nothing -> do
          status status404
          respondText "card does not exist"
        Just image -> do
          addHeader "Content-Type" "image/png"
          respondBytes image
  onGet "/api/tcg/binder/:userid" do
    userid <- pathParam "userid"
    cards <- DB.lrange a.db ("tcg-inventory:" <> userid) 0 (-1)
    respondHTML do
      head_ do
        title_ "LCOLONQ: The Game"
      body_ do
        forM_ cards $ \c -> do
          img_ [src_ $ mconcat ["/api/tcg/card/", decodeUtf8 c, ".png"]]

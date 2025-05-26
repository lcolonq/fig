module Fig.Web.Module.Sentiment
  ( public
  ) where

import Fig.Prelude

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: Module
public a = do
  onGet "/api/sentiment" do
    s <- DB.get a.db "sentiment" >>= \case
      Nothing -> pure "0"
      Just x -> pure x
    respondText $ decodeUtf8 s
  onPost "/api/sentiment/green" do
    DB.incr a.db "sentiment"
  onPost "/api/sentiment/red" do
    DB.decr a.db "sentiment"

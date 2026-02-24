module Fig.Web.Module.Sentiment
  ( public
  ) where

import Fig.Prelude

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/sentiment" do
    s <- DB.run a.db (DB.get "sentiment") >>= \case
      Nothing -> pure "0"
      Just x -> pure x
    respondText $ decodeUtf8 s
  onPost "/api/sentiment/green" do
    DB.run a.db $ DB.incr "sentiment"
  onPost "/api/sentiment/red" do
    DB.run a.db $ DB.decr "sentiment"

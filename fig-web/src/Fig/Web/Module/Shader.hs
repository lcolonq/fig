module Fig.Web.Module.Shader
  ( public
  ) where

import Fig.Prelude

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/shader" do
    DB.run a.db (DB.get "shader") >>= \case
      Nothing -> do
        status status404
        respondText "no shader present"
      Just sh -> respondText $ decodeUtf8 sh

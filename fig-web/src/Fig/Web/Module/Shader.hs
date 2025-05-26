module Fig.Web.Module.Shader
  ( public
  ) where

import Fig.Prelude

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: Module
public a = do
  onGet "/api/shader" do
    DB.get a.db "shader" >>= \case
      Nothing -> do
        status status404
        respondText "no shader present"
      Just sh -> respondText $ decodeUtf8 sh

module Fig.Web.Module.Debt
  ( public
  ) where

import Fig.Prelude

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/debt" do
    debts <- DB.run a.db $ DB.hgetall "debt"
    respondJSON
      $ Map.fromList
      $ mapMaybe (\(k, v) -> (decodeUtf8 k,) <$> readMaybe @Double (unpack $ decodeUtf8 v))
      $ Map.toList debts

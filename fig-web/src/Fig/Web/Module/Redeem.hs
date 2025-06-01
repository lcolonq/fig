module Fig.Web.Module.Redeem
  ( secure
  ) where

import Fig.Prelude

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import Fig.Web.Utils
import Fig.Web.Types
import Fig.Web.Auth

secure :: SecureModule
secure a = do
  onGet "/api/redeeminfo" do
    respondText "hiiiiiii"
  onPost "/api/redeem" $ authed a \creds -> do
    name <- formParam "name"
    input <- formParamMaybe "input"
    log $ creds.user <> " redeemed: " <> name
    liftIO . a.cmds.publish "frontend redeem incoming"
      . encodeUtf8 . Text.intercalate "\t" $
      [ creds.user
      , name
      ] <> Maybe.maybeToList input
    respondText "it worked"

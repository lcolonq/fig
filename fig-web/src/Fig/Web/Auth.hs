module Fig.Web.Auth
  ( Credentials(..)
  , authed
  ) where

import Fig.Prelude

import qualified Data.Text as Text
import qualified Web.Scotty as Sc

import Fig.Web.Types
import Fig.Web.Utils

data Credentials = Credentials
  { user :: Text
  , twitchId :: Text
  }
authed :: SecureModuleArgs -> (Credentials -> Sc.ActionM ()) -> Sc.ActionM ()
authed args h | args.options.simAuth = do
  let auth = Credentials { user = "lcolonq", twitchId = "866686220" }
  h auth
authed _ h = do
  muser <- header "Remote-User"
  memail <- header "Remote-Email"
  case (muser, memail) of
    (Just user, Just email) | twitchId:_ <- Text.splitOn "@" email -> do
      let auth = Credentials{..}
      h auth
    _else -> do
      status status401
      respondText "you're not logged in buddy (this is probably a bug, go message clonk)"

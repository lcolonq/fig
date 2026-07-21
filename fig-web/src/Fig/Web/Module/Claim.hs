module Fig.Web.Module.ShindigsSorting
  ( secure
  ) where

import Fig.Prelude

import Fig.Web.Utils
import Fig.Web.Types
import Fig.Web.Auth
import qualified Fig.Utils.DB as DB

secure :: SecureModule
secure a = do
  onPost "/api/claim/:code" $ authed a \creds -> do
    code <- pathParam "code"
    DB.run a.db (DB.hget code "claim:codes") >>= \case
      Nothing -> log $ "user " <> creds.user <> " tried invalid badge claim code: " <> decodeUtf8 code
      Just badge -> do
        log $ "user " <> creds.user <> " redeemed valid badge claim code: " <> decodeUtf8 code
        let bstid = encodeUtf8 creds.twitchId
        DB.run a.db do
          DB.sadd ("user:badges:" <> bstid) [badge]

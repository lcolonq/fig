module Fig.Frontend.Auth where

import Fig.Prelude

import qualified Network.HTTP.Req as R

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Jose.Jwk as Jwk
import qualified Jose.Jwt as Jwt

import qualified Web.Scotty as Sc
import qualified Web.Scotty.Cookie as Sc.C

import Fig.Frontend.Utils

data TokenContents = TokenContents
  { aud :: !Text
  , exp :: !Int
  , iat :: !Int
  , iss :: !Text
  , sub :: !Text
  , azp :: !Text
  , nonce :: !Text
  , preferred_username :: !Text
  } deriving (Show, Eq, Generic)
instance Aeson.FromJSON TokenContents

fetchJwk :: MonadIO m => m (Maybe Jwk.Jwk)
fetchJwk = do
  resp <- R.responseBody <$> R.runReq R.defaultHttpConfig do
    R.req R.GET (R.https "id.twitch.tv" R./: "oauth2" R./: "keys") R.NoReqBody R.jsonResponse mempty
  let mkeys = Aeson.parseMaybe (Aeson..: "keys") resp
  let mjwk = mkeys >>= headMay
  log $ tshow mjwk
  pure mjwk

validateToken :: MonadIO m => ByteString -> m (Maybe TokenContents)
validateToken encodedToken = fetchJwk >>= \case
  Nothing -> pure Nothing
  Just jwk -> liftIO (Jwt.decode [jwk] Nothing encodedToken) >>= \case
    Left err -> do
      log $ "Failed to decode token: " <> tshow err
      pure Nothing
    Right jwt -> do
      let contents = case jwt of
            Jwt.Unsecured bs -> bs
            Jwt.Jws (_, bs) -> bs
            Jwt.Jwe (_, bs) -> bs
      log $ tshow contents
      pure $ Aeson.decodeStrict contents

data Auth = Auth { id :: !Text, name :: !Text } deriving Show
checkAuth :: Config -> Sc.ActionM (Maybe Auth)
checkAuth cfg = (,)
  <$> Sc.C.getCookie "id_token"
  <*> Sc.C.getCookie "authnonce"
  >>= \case
    (Just token, Just nonce) -> do
      validateToken (encodeUtf8 token) >>= \case
        Just tc 
          | tc.aud == cfg.clientId
          , tc.nonce == nonce
            -> do
              log $ tshow tc
              pure . Just $ Auth
                { name = tc.preferred_username
                , id = tc.sub
                }
        _else -> pure Nothing
    _else -> pure Nothing

authed :: Config -> (Auth -> Sc.ActionM ()) -> Sc.ActionM ()
authed cfg f = checkAuth cfg >>= \case
  Nothing -> do
    Sc.status status401
    Sc.text "unauthorized"
  Just auth -> f auth

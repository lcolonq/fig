module Fig.Frontend.Auth where

import Fig.Prelude

import GHC.Generics (Generic)

import qualified Network.HTTP.Req as R

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Web.Twain as Tw

import qualified Jose.Jwk as Jwk
import qualified Jose.Jwt as Jwt

import Fig.Frontend.Utils

data TokenContents = TokenContents
  { aud :: Text
  , exp :: Int
  , iat :: Int
  , iss :: Text
  , sub :: Text
  , azp :: Text
  , nonce :: Text
  , preferred_username :: Text
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

data Auth = Auth { id :: Text, name :: Text } deriving Show
checkAuth :: Config -> Tw.ResponderM (Maybe Auth)
checkAuth cfg = (,)
  <$> Tw.cookieParamMaybe "id_token"
  <*> Tw.cookieParamMaybe "authnonce"
  >>= \case
    (Just token, Just nonce) -> do
      validateToken token >>= \case
        Just tc 
          | tc.aud == cfg.clientId
          , tc.nonce == nonce
            -> do
              log $ tshow tc
              pure . Just $ Auth
                { name = tc.preferred_username
                , id = tc.sub
                }
        _ -> pure Nothing
    _ -> pure Nothing

authed :: Config -> (Auth -> Tw.ResponderM a) -> Tw.ResponderM a
authed cfg f = checkAuth cfg >>= \case
  Nothing -> Tw.send . Tw.status Tw.status401 $ Tw.text "unauthorized"
  Just auth -> f auth

{-# Language QuasiQuotes #-}

module Fig.Frontend where

import Fig.Prelude

import Control.Lens (use)

import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as BS.Base64

import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Twain as Tw

import Fig.Utils.SExpr
import Fig.Bus.Client
import Fig.Frontend.Utils
import Fig.Frontend.Auth
import Fig.Frontend.State
import qualified Fig.Frontend.DB as DB

server :: Config -> (Text, Text) -> IO ()
server cfg busAddr = do
  log $ "Frontend server running on port " <> tshow cfg.port
  busClient busAddr
    (\cmds -> do
        log "Connected to bus!"
        Warp.run cfg.port =<< app cfg cmds
    )
    (\_ _ -> pure ())
    (pure ())

sexprStr :: Text -> SExpr
sexprStr = SExprString . BS.Base64.encodeBase64 . encodeUtf8

app :: Config -> Commands IO -> IO Tw.Application
app cfg cmds = do
  log "Connecting to database..."
  db <- DB.connect cfg
  log "Connected! Server active."
  st <- stateRef
  pure $ foldr' @[] ($)
    (Tw.notFound . Tw.send $ Tw.text "not found")
    [ Wai.Static.staticPolicy $ Wai.Static.addBase cfg.assetPath
    , Tw.get "/api/check" $ authed cfg \auth -> do
        Tw.send $ Tw.json @[Text] [auth.id, auth.name]
    , Tw.put "/api/buffer" do
        buf <- withState st $ use buffer 
        Tw.send $ Tw.text buf
    , Tw.get "/api/user/:name" do
        name <- Text.toLower <$> Tw.param "name"
        DB.get db ("user:" <> encodeUtf8 name) >>= \case
          Nothing -> Tw.send . Tw.status Tw.status404 $ Tw.text "user not found"
          Just val -> Tw.send . Tw.text $ decodeUtf8 val
    , Tw.post "/api/redeem/:name" do
        me <- Text.toLower <$> Tw.param "ayem"
        name <- Text.toLower <$> Tw.param "name"
        input <- Tw.paramMaybe "input"
        liftIO $ cmds.publish [sexp|(frontend redeem incoming)|]
          $ mconcat
            [ [ sexprStr me
              , sexprStr name
              ]
            , maybe [] ((:[]) . sexprStr) input
            ]
        Tw.send $ Tw.text "it worked"
    , Tw.get "/api/songs" do
        DB.hvals db "songnames" >>= \case
          Nothing -> Tw.send . Tw.status Tw.status404 $ Tw.text "no sounds found :("
          Just songs -> Tw.send . Tw.text . pretty . SExprList @Void $ SExprString . decodeUtf8 <$> songs
    , Tw.get "/api/song/:hash" do
        hash <- Tw.param "hash"
        DB.hget db "songnotes" hash >>= \case
          Nothing -> Tw.send . Tw.status Tw.status404 $ Tw.text "song not found"
          Just val -> Tw.send . Tw.text $ decodeUtf8 val
    , Tw.get "/api/poke/:name" do
        target <- encodeUtf8 . Text.toLower <$> Tw.param "name"
        inbox <- fromMaybe [] <$> DB.smembers db ("pokeinbox:" <> target)
        Tw.send . Tw.text . pretty . SExprList @Void $ sexprStr . decodeUtf8 <$> inbox
    , Tw.post "/api/poke/:name" do
        me <- encodeUtf8 . Text.toLower <$> Tw.param "ayem"
        target <- encodeUtf8 . Text.toLower <$> Tw.param "name"
        DB.sismember db ("pokeinbox:" <> me) target >>= \case
          True -> do
            log . tshow $ "handshake between " <> me <> " and " <> target <> " complete!"
            DB.srem db ("pokeinbox:" <> target) [me]
            DB.srem db ("pokeinbox:" <> me) [target]
            Tw.send $ Tw.text "complete"
          False -> do
            log . tshow $ "partial handshake from " <> me <> " to " <> target
            DB.sadd db ("pokeinbox:" <> target) [me]
            Tw.send $ Tw.text "partial"
    ]

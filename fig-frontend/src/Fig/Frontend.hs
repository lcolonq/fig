module Fig.Frontend where

import Fig.Prelude

import Control.Lens (use)

import Data.Text (toLower)

-- import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Twain as Tw

import qualified Lucid as L
import qualified Lucid.Base as L

import Fig.Utils.SExpr
import Fig.Frontend.Utils
import Fig.Frontend.Auth
import Fig.Frontend.State
import qualified Fig.Frontend.DB as DB

server :: Config -> IO ()
server cfg = do
  log $ "Frontend server running on port " <> tshow cfg.port
  Warp.run cfg.port =<< app cfg

window :: Text -> Text -> L.Html () -> L.Html ()
window id_ title body = 
  L.term "fig-window" [L.id_ id_, L.makeAttributes "title" title] do
    body

app :: Config -> IO Tw.Application
app cfg = do
  db <- DB.connect
  st <- stateRef
  pure $ foldr' @[] ($)
    (Tw.notFound . Tw.send $ Tw.text "not found")
    -- [ Wai.Static.staticPolicy $ Wai.Static.addBase cfg.assetPath
    [ Tw.get "/"
      . Tw.send . Tw.html
      . L.renderBS
      $ L.doctypehtml_ do
          L.head_ do
            L.title_ "clonk zone api home page"
            L.link_ [L.rel_ "icon", L.href_ "data:;base64,iVBORw0KGgo="]
          L.body_ do
            "hello"
    , Tw.get "/api/check" $ authed cfg \auth -> do
        Tw.send $ Tw.json @[Text] [auth.id, auth.name]
    , Tw.put "/api/buffer" do
        buf <- withState st $ use buffer 
        Tw.send $ Tw.text buf
    , Tw.get "/api/user/:name" do
        name <- toLower <$> Tw.param "name"
        DB.get db ("user:" <> encodeUtf8 name) >>= \case
          Nothing -> Tw.send . Tw.status Tw.status404 $ Tw.text "user not found"
          Just val -> Tw.send . Tw.text $ decodeUtf8 val
    , Tw.get "/api/songs" do
        DB.hvals db "songnames" >>= \case
          Nothing -> Tw.send . Tw.status Tw.status404 $ Tw.text "no sounds found :("
          Just songs -> Tw.send . Tw.text . pretty . SExprList @Void $ SExprString . decodeUtf8 <$> songs
    ]

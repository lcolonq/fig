module Fig.Frontend where

import Fig.Prelude

import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Twain as Tw

import qualified Lucid as L

import Fig.Frontend.Utils
import Fig.Frontend.Auth

server :: Config -> IO ()
server cfg = do
  log $ "Frontend server running on port " <> tshow cfg.port
  Warp.run cfg.port $ app cfg

app :: Config -> Tw.Application
app cfg = foldr' @[] ($)
  (Tw.notFound . Tw.send $ Tw.text "not found")
  [ Wai.Static.staticPolicy $ Wai.Static.addBase cfg.assetPath
  , Tw.get "/"
    . Tw.send . Tw.html
    . L.renderBS
    $ L.doctypehtml_ do
        L.head_ do
          L.title_ "The Junkyard"
          L.link_ [L.rel_ "stylesheet", L.href_ "js/index.css"]
          L.link_ [L.rel_ "stylesheet", L.href_ "https://fonts.googleapis.com/css?family=Rubik+Maps"]
          L.link_ [L.rel_ "icon", L.href_ "data:;base64,iVBORw0KGgo="]
          L.script_ [L.type_ "module", L.src_ "js/index.js"] ("" :: L.Html ())
        L.body_ do
          L.term "fig-backdrop" ""
          L.term "fig-header" ""
          L.term "fig-login" ""
          L.term "fig-window" do
            L.h1_ "hello"
  , Tw.get "/api/check" $ authed cfg \auth -> do
      Tw.send $ Tw.json @[Text] [auth.id, auth.name]
  ]

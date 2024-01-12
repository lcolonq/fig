module Fig.Frontend where

import Fig.Prelude

import Control.Lens (use)

import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Twain as Tw

import qualified Lucid as L
import qualified Lucid.Base as L

import Fig.Frontend.Utils
import Fig.Frontend.Auth
import Fig.Frontend.State

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
  st <- stateRef
  pure $ foldr' @[] ($)
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
            L.term "fig-footer" ""
            window "test1" "window one" do
              L.p_ "Deserunt consequatur neque autem. Dicta assumenda autem consequatur sunt animi. Dolor voluptatem rerum aut id ut vel. Labore soluta itaque voluptas dolores repellat. Voluptatem dolor fugit necessitatibus amet et natus velit. Enim nihil voluptate qui quasi architecto. Reprehenderit porro aperiam eaque sequi veritatis in quos. Odio tenetur labore cupiditate nisi. Deserunt aut dolore consequuntur inventore quod veniam commodi. Incidunt qui sequi dolor. Quia adipisci dolores ab. Dolor molestias est earum ea. Possimus ullam repellat qui consequatur dolorem excepturi non. Incidunt magnam quaerat temporibus quisquam. Vero nihil possimus ratione voluptas sunt. Qui quisquam ipsa omnis est totam iure odio. Ab fugiat in id nisi. Dolor sit est soluta eaque ut eveniet. Eveniet rerum fuga doloremque repellendus eligendi sunt asperiores. Tenetur iure vitae ea sapiente et. Aspernatur natus aut rerum magnam occaecati. Veritatis exercitationem necessitatibus assumenda est assumenda eaque molestiae."
            window "test2" "window two" do
              L.h1_ "hello"
            L.term "fig-gizmo" [L.id_ "gizmo1"] ""
    , Tw.get "/api/check" $ authed cfg \auth -> do
        Tw.send $ Tw.json @[Text] [auth.id, auth.name]
    , Tw.put "/api/buffer" do
        buf <- withState st $ use buffer 
        Tw.send $ Tw.text buf
    ]

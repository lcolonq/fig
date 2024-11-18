{-# Language QuasiQuotes #-}

module Fig.Web.Secure where

import Fig.Prelude

import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as BS.Base64
import qualified Data.Set as Set

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Scotty as Sc

import Fig.Utils.SExpr
import Fig.Bus.Client
import Fig.Web.Utils
import qualified Fig.Web.DB as DB

data LiveEvent
  = LiveEventOnline !(Set.Set Text)
  | LiveEventOffline !(Set.Set Text)
  deriving (Show, Eq, Ord)

server :: Config -> (Text, Text) -> IO ()
server cfg busAddr = do
  log $ "Web server running on port " <> tshow cfg.port
  busClient busAddr
    (\cmds -> do
        log "Connected to bus!"
        Warp.run cfg.port =<< app cfg cmds
    )
    (\_cmds d -> do
        log $ "Invalid event: " <> tshow d
    )
    (pure ())

sexprStr :: Text -> SExpr
sexprStr = SExprString . BS.Base64.encodeBase64 . encodeUtf8

app :: Config -> Commands IO -> IO Wai.Application
app cfg cmds = do
  log "Connecting to database..."
  _db <- DB.connect cfg
  log "Connected! Secure server active."
  Sc.scottyApp do
    Sc.middleware . Wai.Static.staticPolicy $ mconcat
      [ Wai.Static.isNotAbsolute
      , Wai.Static.only
        [ ("menu", "menu.html")
        , ("main.css", "main.css")
        , ("main.js", "main.js")
        ] Wai.Static.<|> Wai.Static.hasPrefix "assets"
      , Wai.Static.addBase cfg.assetPath
      ]
    Sc.get "/" do
      Sc.text "this is the secure endpoint"
    Sc.post "/api/redeem" do
      me <- Text.toLower <$> Sc.formParam "ayem"
      name <- Sc.formParam "name"
      input <- Sc.formParamMaybe "input"
      liftIO $ cmds.publish [sexp|(frontend redeem incoming)|]
        $ mconcat
          [ [ sexprStr me
            , sexprStr name
            ]
          , maybe [] ((:[]) . sexprStr) input
          ]
      Sc.text "it worked"
    Sc.notFound do
      Sc.text "not found"

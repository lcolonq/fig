{-# Language QuasiQuotes #-}

module Fig.Web.Secure where

import Fig.Prelude

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
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
import qualified Fig.Web.Exchange as Exchange

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
  db <- DB.connect cfg
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
    Sc.get "/api/status" do
      Sc.text "this is the secure endpoint"
    Sc.post "/api/redeem" do
      muser <- Sc.header "Remote-User"
      memail <- Sc.header "Remote-Email"
      case (muser, memail) of
        (Just user, Just _email) -> do
          name <- Sc.formParam "name"
          input <- Sc.formParamMaybe "input"
          liftIO $ cmds.publish [sexp|(frontend redeem incoming)|]
            $ mconcat
              [ [ sexprStr $ Text.Lazy.toStrict user
                , sexprStr name
                ]
              , maybe [] ((:[]) . sexprStr) input
              ]
          Sc.text "it worked"
        _else -> do
          Sc.status status401
          Sc.text "you're not logged in buddy"
    Sc.post "/api/exchange" do
      Sc.header "Remote-Email" >>= \case
        Nothing -> do
          Sc.status status401
          Sc.text "you're not logged in buddy"
        Just creator -> do
          haveCur <- Text.Lazy.toStrict <$> Sc.formParam "haveCur"
          haveAmount <- Sc.formParam "haveAmount"
          wantCur <- Text.Lazy.toStrict <$> Sc.formParam "wantCur"
          wantAmount <- Sc.formParam "wantAmount"
          key <- Exchange.createOrder db $ Exchange.Order
            { creator = Text.Lazy.toStrict creator
            , haveCur = haveCur
            , haveAmount = haveAmount
            , wantCur = wantCur
            , wantAmount = wantAmount
            }
          Sc.text . Text.Lazy.fromStrict $ decodeUtf8 key
    Sc.post "/api/exchange/:key" do
      Sc.header "Remote-Email" >>= \case
        Nothing -> do
          Sc.status status401
          Sc.text "you're not logged in buddy"
        Just buyer -> do
          key <- Sc.pathParam "key"
          Exchange.satisfyOrder db key $ Text.Lazy.toStrict buyer
    Sc.delete "/api/exchange/:key" do
      Sc.header "Remote-Email" >>= \case
        Nothing -> do
          Sc.status status401
          Sc.text "you're not logged in buddy"
        Just _buyer -> do
          key <- Sc.pathParam "key"
          Exchange.cancelOrder db key
    Sc.notFound do
      Sc.text "not found"

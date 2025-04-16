{-# Language QuasiQuotes #-}

module Fig.Web where

import Fig.Prelude

import System.Random (randomRIO)

import Control.Monad (unless)
import Control.Lens (use, (^?), Ixed (..))
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.L
import qualified Data.ByteString.Base64 as BS.Base64
import qualified Data.Set as Set

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

import qualified Web.Scotty as Sc

import Fig.Utils.SExpr
import Fig.Bus.Client
import Fig.Web.Utils
import Fig.Web.Auth
import Fig.Web.State
import qualified Fig.Web.DB as DB
import qualified Fig.Web.LDAP as LDAP
import qualified Fig.Web.Exchange as Exchange

data LiveEvent
  = LiveEventOnline !(Set.Set Text)
  | LiveEventOffline !(Set.Set Text)
  deriving (Show, Eq, Ord)

data Channels = Channels
  { live :: !(Chan.Chan LiveEvent)
  , gizmo :: !(Chan.Chan Text)
  }

newChannels :: IO Channels
newChannels = do
  live <- Chan.newChan
  gizmo <- Chan.newChan
  pure Channels {..}

data Globals = Globals
  { currentlyLive :: !(MVar.MVar (Set.Set Text))
  }

newGlobals :: IO Globals
newGlobals = do
  currentlyLive <- MVar.newMVar Set.empty
  pure Globals {..}
  
server :: Config -> (Text, Text) -> IO ()
server cfg busAddr = do
  log $ "Web server running on port " <> tshow cfg.port
  chans <- newChannels
  globs <- newGlobals
  busClient busAddr
    (\cmds -> do
        log "Connected to bus!"
        cmds.subscribe [sexp|(monitor twitch stream online)|]
        cmds.subscribe [sexp|(gizmo buffer update)|]
        Warp.run cfg.port =<< app cfg cmds chans globs.currentlyLive
    )
    (\_cmds d -> do
        case d of
          SExprList (ev:rest)
            | ev == [sexp|(monitor twitch stream online)|] -> do
                let live = mapMaybe (\case SExprString s -> Just s; _other -> Nothing) rest
                let new = Set.fromList live 
                old <- MVar.swapMVar globs.currentlyLive new
                let online = Set.difference new old
                let offline = Set.difference old new
                log $ "Newly online: " <> Text.intercalate " " (Set.toList online) <> ", newly offline: " <> Text.intercalate " " (Set.toList offline)
                unless (Set.null online) . Chan.writeChan chans.live $ LiveEventOnline online
                unless (Set.null offline) . Chan.writeChan chans.live $ LiveEventOnline offline
            | ev == [sexp|(gizmo buffer update)|] -> do
                let updates = mapMaybe (\case SExprString s -> Just s; _other -> Nothing) rest
                forM_ updates $ Chan.writeChan chans.gizmo
          _other -> log $ "Invalid event: " <> tshow d
    )
    (pure ())

sexprStr :: Text -> SExpr
sexprStr = SExprString . BS.Base64.encodeBase64 . encodeUtf8

app :: Config -> Commands IO -> Channels -> MVar.MVar (Set.Set Text) -> IO Wai.Application
app cfg _cmds chans currentlyLive = do
  log "Connecting to database..."
  db <- DB.connect cfg
  log "Connected! Server active."
  st <- stateRef
  Sc.scottyApp do
    Sc.middleware . Wai.Static.staticPolicy $ mconcat
      [ Wai.Static.isNotAbsolute
      , Wai.Static.only
        [ ("register", "register.html")
        , ("gizmo", "gizmo.html")
        , ("main.css", "main.css")
        , ("main.js", "main.js")
        ] Wai.Static.<|> Wai.Static.hasPrefix "assets"
      , Wai.Static.addBase cfg.assetPath
      ]
    -- Sc.get "/register" do
    --   Sc.redirect "/register.html"
    Sc.get "/unauthorized" do
      Sc.status status401
      Sc.text $ mconcat
        [ "your request was rejected because that endpoint requires authentication\n"
        , "you can log in by POSTing your credentials to https://auth.colonq.computer/api/firstfactor\n"
        , "for example:\n"
        , "  curl https://auth.colonq.computer/api/firstfactor \\\n"
        , "    --header \"Content-Type: application/json\" \\\n"
        , "    --request POST \\\n"
        , "    --data '{\"username\":\"AzureDiamond\",\"password\":\"hunter2\"}' \\\n"
        , "    --cookie-jar cookies.txt\n"
        , "this will write a cookie called \"authelia_session\" to cookies.txt\n"
        , "send this cookie along with your requests to use the secure endpoints\n"
        , "for example:\n"
        , "  curl https://secure.colonq.computer --cookie cookies.txt\n"
        ]
    Sc.get "/api/register" $ authed cfg \auth -> do
      log "Authenticated with Twitch, trying to register..."
      let user = Text.toLower auth.name
      LDAP.resetUserPassword cfg user auth.id >>= \case
        Nothing -> do
          log "Failed to register user"
          Sc.status status500
          Sc.text "failed to register"
        Just pass -> do
          log "Successfully registered user, responding..."
          Sc.text . Text.L.fromStrict $ user <> " " <> pass
    Sc.get "/api/check" $ authed cfg \auth -> do
      Sc.json @[Text] [auth.id, auth.name]
    Sc.put "/api/buffer" do
      buf <- withState st $ use buffer 
      Sc.text $ Text.L.fromStrict buf
    Sc.get "/api/motd" do
      DB.get db "motd" >>= \case
        Nothing -> Sc.text ""
        Just val -> Sc.text . Text.L.fromStrict $ decodeUtf8 val
    Sc.get "/api/catchphrase" do
      let catchphrases =
            [ "vtuber (male)"
            , "man of letters"
            , "cool guy, online"
            , "internet clown man"
            , "professional emacs fan"
            , "web freak"
            , "guy who really likes programming"
            , "i use nixos btw"
            , "(are these funny or cringe or both?)"
            , "haha yay"
            , "Joel"
            ] :: [Text]
      i <- randomRIO (0, length catchphrases - 1)
      case catchphrases ^? ix i of
        Nothing -> Sc.text "man of letters"
        Just val -> Sc.text $ Text.L.fromStrict val
    Sc.get "/api/user/:name" do
      name <- Text.toLower <$> Sc.pathParam "name"
      DB.get db ("user:" <> encodeUtf8 name) >>= \case
        Nothing -> do
          Sc.status status404
          Sc.text "user not found"
        Just val -> Sc.text . Text.L.fromStrict $ decodeUtf8 val
    Sc.get "/api/songs" do
      DB.hvals db "songnames" >>= \case
        Nothing -> do
          Sc.status status404
          Sc.text "no sounds found :("
        Just songs -> Sc.text . Text.L.fromStrict . pretty . SExprList @Void $ SExprString . decodeUtf8 <$> songs
    Sc.get "/api/song/:hash" do
      hash <- Sc.pathParam "hash"
      DB.hget db "songnotes" hash >>= \case
        Nothing -> do
          Sc.status status404
          Sc.text "song not found"
        Just val -> Sc.text . Text.L.fromStrict $ decodeUtf8 val
    Sc.get "/api/poke/:name" do
      target <- encodeUtf8 . Text.toLower <$> Sc.pathParam "name"
      inbox <- fromMaybe [] <$> DB.smembers db ("pokeinbox:" <> target)
      Sc.text . Text.L.fromStrict . pretty . SExprList @Void $ sexprStr . decodeUtf8 <$> inbox
    Sc.post "/api/poke/:name" do
      me <- encodeUtf8 . Text.toLower <$> Sc.formParam "ayem"
      target <- encodeUtf8 . Text.toLower <$> Sc.pathParam "name"
      DB.sismember db ("pokeinbox:" <> me) target >>= \case
        True -> do
          log . tshow $ "handshake between " <> me <> " and " <> target <> " complete!"
          DB.srem db ("pokeinbox:" <> target) [me]
          DB.srem db ("pokeinbox:" <> me) [target]
          Sc.text "complete"
        False -> do
          log . tshow $ "partial handshake from " <> me <> " to " <> target
          DB.sadd db ("pokeinbox:" <> target) [me]
          Sc.text "partial"
    Sc.get "/api/sentiment" do
      s <- DB.get db "sentiment" >>= \case
        Nothing -> pure "0"
        Just x -> pure x
      Sc.text . Text.L.fromStrict . decodeUtf8 $ s
    Sc.post "/api/sentiment/green" do
      DB.incr db "sentiment"
    Sc.post "/api/sentiment/red" do
      DB.decr db "sentiment"
    Sc.get "/api/shader" do
      DB.get db "shader" >>= \case
        Nothing -> do
          Sc.status status404
          Sc.text "no shader present"
        Just sh -> Sc.text . Text.L.fromStrict $ decodeUtf8 sh
    Sc.get "/api/exchange" do
      listings <- Exchange.getOrders db
      Sc.json listings
    Sc.get "/api/gizmo" do
      buf <- Sc.queryParam "buf"
      DB.hget db "gizmos" buf >>= \case
        Nothing -> do
          Sc.status status404
          Sc.text "gizmo does not exist"
        Just html -> Sc.html . Text.L.fromStrict $ decodeUtf8 html
    Sc.get "/api/gizmo/list" do
      gizmos <- maybe [] (fmap decodeUtf8) <$> DB.hkeys db "gizmos"
      Sc.text $ Text.L.fromStrict $ Text.unlines gizmos
    websocket "/api/gizmo/events" \conn -> do
      c <- Chan.dupChan chans.gizmo
      forever do
        ev <- liftIO $ Chan.readChan c
        WS.sendTextData conn ev
    Sc.get "/api/circle" do
      live <- liftIO $ MVar.readMVar currentlyLive
      Sc.text . Text.L.fromStrict . pretty . SExprList @Void $ SExprString <$> Set.toList live
    websocket "/api/circle/events" \conn -> do
      c <- Chan.dupChan chans.live
      forever do
        ev <- liftIO $ Chan.readChan c
        WS.sendTextData conn $ case ev of
          LiveEventOnline online ->
            pretty $ SExprList @Void
            [ SExprString "online"
            , SExprList $ SExprString <$> Set.toList online
            ]
          LiveEventOffline offline ->
            pretty $ SExprList @Void
            [ SExprString "offline"
            , SExprList $ SExprString <$> Set.toList offline
            ]
    Sc.notFound do
      Sc.text "not found"

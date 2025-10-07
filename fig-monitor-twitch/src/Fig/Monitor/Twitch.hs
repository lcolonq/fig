{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Fig.Monitor.Twitch
  ( twitchEventClient
  , twitchChatClient
  , twitchChannelLiveMonitor
  , twitchEndpointTest
  , userTokenRedirectServer
  ) where

import Fig.Prelude

import Control.Concurrent (threadDelay)

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Default.Class (def)

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import qualified Wuss as WS
import qualified Network.WebSockets.Connection as WS

import Network.Wai.Handler.Warp (setPort)
import qualified Web.Scotty as Scotty

import Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status as HTTP

import Fig.Bus.Binary.Client
import Fig.Monitor.Twitch.Utils

loginToMaybeUserId :: Text -> Authed (Maybe Text)
loginToMaybeUserId login = do
  res <- authedRequestJSON @() "GET" ("https://api.twitch.tv/helix/users?login=" <> login) Nothing
  let mid = flip Aeson.parseMaybe res \obj -> do
        obj .: "data" >>= \case
          Aeson.Array ((V.!? 0) -> Just (Aeson.Object d)) -> d .: "id"
          _else -> mempty
  pure mid

loginToUserId :: Text -> Authed Text
loginToUserId login = do
  res <- authedRequestJSON @() "GET" ("https://api.twitch.tv/helix/users?login=" <> login) Nothing
  let mid = flip Aeson.parseMaybe res \obj -> do
        obj .: "data" >>= \case
          Aeson.Array ((V.!? 0) -> Just (Aeson.Object d)) -> d .: "id"
          _else -> mempty
  maybe (throwM $ FigMonitorTwitchException "Failed to extract user ID") pure mid

usersAreLive :: [Text] -> Authed (Set.Set Text)
usersAreLive users = do
  log $ "Checking liveness for: " <> Text.intercalate " " users
  res <- authedRequestJSON @()
    "GET"
    ( mconcat
      [ "https://api.twitch.tv/helix/streams?type=live"
      , mconcat $ ("&user_login="<>) <$> users
      ]
    )
    Nothing
  let mos = flip Aeson.parseEither res \obj -> do
        obj .: "data" >>= \case
          Aeson.Array os -> catMaybes . toList <$> forM os \case
            Aeson.Object o -> Just <$> o .: "user_login"
            _else -> pure Nothing
          _else -> mempty
  case mos of
    Left err -> throwM $ FigMonitorTwitchException $ "Failed to check liveness: " <> pack err <> "\nResponse was: " <> tshow res
    Right os -> pure . Set.fromList $ filter (`elem` os) users

subscribe :: Text -> Text -> Text -> Authed ()
subscribe sessionId event user = do
  log $ "Subscribing to " <> event <> " events for user ID: " <> user
  res <- authedRequestJSON "POST" "https://api.twitch.tv/helix/eventsub/subscriptions" . Just $ Aeson.object
    [ "type" .= event
    , "version" .= ("1" :: Text)
    , "condition" .= Aeson.object
      [ "broadcaster_user_id" .= user
      ]
    , "transport" .= Aeson.object
      [ "method" .= ("websocket" :: Text)
      , "session_id" .= sessionId
      ]
    ]
  case Aeson.parseMaybe (.: "total_cost") res of
    Just (_ :: Int) -> pure ()
    _else -> throwM $ FigMonitorTwitchException "Failed to subscribe to event"

subscribeFollows :: Text -> Text -> Authed ()
subscribeFollows sessionId user = do
  log $ "Subscribing to channel.follow events for user ID: " <> user
  res <- authedRequestJSON "POST" "https://api.twitch.tv/helix/eventsub/subscriptions" . Just $ Aeson.object
    [ "type" .= ("channel.follow" :: Text)
    , "version" .= ("2" :: Text)
    , "condition" .= Aeson.object
      [ "broadcaster_user_id" .= user
      , "moderator_user_id" .= user
      ]
    , "transport" .= Aeson.object
      [ "method" .= ("websocket" :: Text)
      , "session_id" .= sessionId
      ]
    ]
  case Aeson.parseMaybe (.: "total_cost") res of
    Just (_ :: Int) -> pure ()
    _else -> throwM $ FigMonitorTwitchException "Failed to subscribe to event"
  
subscribeRaids :: Text -> Text -> Authed ()
subscribeRaids sessionId user = do
  log $ "Subscribing to channel.raid events for user ID: " <> user
  res <- authedRequestJSON "POST" "https://api.twitch.tv/helix/eventsub/subscriptions" . Just $ Aeson.object
    [ "type" .= ("channel.raid" :: Text)
    , "version" .= ("1" :: Text)
    , "condition" .= Aeson.object
      [ "to_broadcaster_user_id" .= user
      ]
    , "transport" .= Aeson.object
      [ "method" .= ("websocket" :: Text)
      , "session_id" .= sessionId
      ]
    ]
  case Aeson.parseMaybe (.: "total_cost") res of
    Just (_ :: Int) -> pure ()
    _else -> throwM $ FigMonitorTwitchException "Failed to subscribe to event"

poll :: Text -> [Text] -> Text -> Authed ()
poll title choices user = do
  log $ "Starting a new poll: \"" <> title <> "\""
  res <- authedRequestJSON "POST" "https://api.twitch.tv/helix/polls" . Just $ Aeson.object
    [ "broadcaster_id" .= user
    , "title" .= title
    , "choices" .= ((\c -> Aeson.object ["title" .= c]) <$> choices)
    , "channel_points_voting_enabled" .= True
    , "channel_points_per_vote" .= (1000 :: Integer)
    , "duration" .= (60 :: Integer)
    ]
  let mid = flip Aeson.parseMaybe res \obj -> do
        obj .: "data" >>= \case
          Aeson.Array ((V.!? 0) -> Just (Aeson.Object d)) -> d .: "id"
          _else -> mempty
  case mid of
    Just (_ :: Text) -> pure ()
    Nothing -> do
      log "Failed to start poll"
      log $ tshow res

createPrediction :: Text -> [Text] -> Text -> Authed ()
createPrediction title choices user = do
  log $ "Starting a new prediction: \"" <> title <> "\""
  res <- authedRequestJSON "POST" "https://api.twitch.tv/helix/predictions" . Just $ Aeson.object
    [ "broadcaster_id" .= user
    , "title" .= title
    , "outcomes" .= ((\c -> Aeson.object ["title" .= c]) <$> choices)
    , "prediction_window" .= (120 :: Integer)
    ]
  let mid = flip Aeson.parseMaybe res \obj -> do
        obj .: "data" >>= \case
          Aeson.Array ((V.!? 0) -> Just (Aeson.Object d)) -> d .: "id"
          _else -> mempty
  case mid of
    Just (_ :: Text) -> pure ()
    Nothing -> log "Failed to start prediction"

finishPrediction :: Text -> Text -> Text -> Authed ()
finishPrediction pid oid user = do
  log $ "Ending prediction: \"" <> pid <> "\""
  res <- authedRequestJSON "PATCH" "https://api.twitch.tv/helix/predictions" . Just $ Aeson.object
    [ "broadcaster_id" .= user
    , "id" .= pid
    , "status" .= ("RESOLVED" :: Text)
    , "winning_outcome_id" .= oid
    ]
  let mid = flip Aeson.parseMaybe res \obj -> do
        obj .: "data" >>= \case
          Aeson.Array ((V.!? 0) -> Just (Aeson.Object d)) -> d .: "id"
          _else -> mempty
  case mid of
    Just (_ :: Text) -> pure ()
    Nothing -> log "Failed to end prediction"

addVIP :: Text -> Text -> Authed ()
addVIP vipuser user = do
  log $ "Adding VIP user: \"" <> vipuser <> "\""
  let body = Aeson.encode $ Aeson.object
        [ "broadcaster_id" .= user
        , "user_id" .= vipuser
        ]
  rc <- ask
  initialRequest <- liftIO . HTTP.parseRequest $ unpack "https://api.twitch.tv/helix/channels/vips"
  let request = initialRequest
        { method = encodeUtf8 "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ ("Authorization", encodeUtf8 $ "Bearer " <> rc.config.userToken)
            , ("Client-Id", encodeUtf8 rc.config.clientId)
            , ("Content-Type", "application/json")
            ]
        }
  response <- liftIO $ HTTP.httpLbs request rc.manager
  unless (HTTP.statusIsSuccessful $ HTTP.responseStatus response) $ do
    log $ "Failed to add VIP: error " <> tshow (HTTP.statusCode $ HTTP.responseStatus response)

removeVIP :: Text -> Text -> Authed ()
removeVIP vipuser user = do
  log $ "Removing VIP user: \"" <> vipuser <> "\""
  let body = Aeson.encode $ Aeson.object
        [ "broadcaster_id" .= user
        , "user_id" .= vipuser
        ]
  rc <- ask
  initialRequest <- liftIO . HTTP.parseRequest $ unpack "https://api.twitch.tv/helix/channels/vips"
  let request = initialRequest
        { method = encodeUtf8 "DELETE"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ ("Authorization", encodeUtf8 $ "Bearer " <> rc.config.userToken)
            , ("Client-Id", encodeUtf8 rc.config.clientId)
            , ("Content-Type", "application/json")
            ]
        }
  response <- liftIO $ HTTP.httpLbs request rc.manager
  unless (HTTP.statusIsSuccessful $ HTTP.responseStatus response) $ do
    log $ "Failed to remove VIP: error " <> tshow (HTTP.statusCode $ HTTP.responseStatus response)

shoutout :: Text -> Text -> Authed ()
shoutout souser user = do
  log $ "Shoutout to: \"" <> souser <> "\""
  let body = Aeson.encode $ Aeson.object
        [ "from_broadcaster_id" .= user
        , "moderator_id" .= user
        , "to_broadcaster_id" .= souser
        ]
  rc <- ask
  initialRequest <- liftIO . HTTP.parseRequest $ unpack "https://api.twitch.tv/helix/chat/shoutouts"
  let request = initialRequest
        { method = encodeUtf8 "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ ("Authorization", encodeUtf8 $ "Bearer " <> rc.config.userToken)
            , ("Client-Id", encodeUtf8 rc.config.clientId)
            , ("Content-Type", "application/json")
            ]
        }
  response <- liftIO $ HTTP.httpLbs request rc.manager
  unless (HTTP.statusIsSuccessful $ HTTP.responseStatus response) $ do
    log $ "Failed to shoutout: error " <> tshow (HTTP.statusCode $ HTTP.responseStatus response)

twitchEndpointTest :: Config -> IO ()
twitchEndpointTest cfg = runAuthed cfg do
  user <- loginToUserId "lcolonq"
  log user

twitchEventClient :: Config -> (Text, Text) -> IO ()
twitchEventClient cfg busAddr = do
  WS.runSecureClient "eventsub.wss.twitch.tv" 443 "/ws" \conn -> do
    welcomeStr <- WS.receiveData conn
    (sessionId :: Text) <- case Aeson.eitherDecodeStrict welcomeStr of
      Left err -> throwM . FigMonitorTwitchException $ tshow err
      Right res -> do
        let mid = flip Aeson.parseMaybe res \obj -> do
              payload <- obj .: "payload"
              session <- payload .: "session"
              session .: "id"
        maybe (throwM $ FigMonitorTwitchException "Failed to extract session ID") pure mid
    log $ "Connected to Twitch API, session ID is: " <> sessionId
    runAuthed cfg do
      user <- loginToUserId cfg.userLogin
      subscribe sessionId "channel.channel_points_custom_reward_redemption.add" user
      subscribe sessionId "channel.prediction.begin" user
      subscribe sessionId "channel.prediction.end" user
      subscribe sessionId "channel.poll.begin" user
      subscribe sessionId "channel.poll.end" user
      subscribe sessionId "channel.subscribe" user
      subscribe sessionId "channel.subscription.gift" user
      subscribeFollows sessionId user
      subscribeRaids sessionId user
    busClient busAddr
      (\cmds -> do
          cmds.subscribe "fig monitor twitch poll create"
          cmds.subscribe "fig monitor twitch prediction create"
          cmds.subscribe "fig monitor twitch prediction finish"
          cmds.subscribe "fig monitor twitch vip add"
          cmds.subscribe "fig monitor twitch vip remove"
          cmds.subscribe "fig monitor twitch shoutout"
          forever do
            resp <- WS.receiveData conn
            case Aeson.eitherDecodeStrict resp of
              Left err -> throwM . FigMonitorTwitchException $ tshow err
              Right res -> case Aeson.parseMaybe ((.: "metadata") >=> (.: "message_type")) res of
                Just ("notification" :: Text) -> case Aeson.parseMaybe ((.: "metadata") >=> (.: "subscription_type")) res of
                  Just ("channel.channel_points_custom_reward_redemption.add" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          nm <- event .: "user_login"
                          reward <- event .: "reward"
                          title <- reward .: "title"
                          minput <- event .:? "user_input"
                          pure (nm, title, minput)
                    case Aeson.parseMaybe parseEvent res of
                      Just (nm, title, minput) -> do
                        log $ "Channel point reward \"" <> title <> "\" redeemed by: " <> nm
                        cmds.publish "fig monitor twitch redeem incoming"
                           . encodeUtf8 . Text.intercalate "\t" $
                           [nm, title] <> Maybe.maybeToList minput
                      _else -> log "Failed to extract payload from channel point redeem event"
                  Just ("channel.prediction.begin" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          pid <- event .: "id"
                          oids <- event .: "outcomes" >>= \case
                            Aeson.Array os -> forM os $ \case
                              Aeson.Object out -> (,) <$> (out .: "title") <*> (out .: "id")
                              _else -> mempty
                            _else -> mempty
                          pure (pid, oids)
                    case Aeson.parseMaybe parseEvent res of
                      Just (pid, oids) -> do
                        log $ "Prediction begin: " <> pid
                        cmds.publish "fig monitor twitch prediction begin"
                           . encodeUtf8 . Text.unwords $
                           [ pid ] <> ((\(title, oid) -> title <> "," <> oid) <$> toList oids)
                      _else -> log "Failed to extract ID from payload for prediction begin event"
                  Just ("channel.prediction.end" :: Text) -> do
                    log "Prediction end"
                    cmds.publish "fig monitor twitch prediction end" ""
                  Just ("channel.raid" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          event .: "from_broadcaster_user_login"
                    case Aeson.parseMaybe parseEvent res of
                      Just nm -> do
                        log $ "Incoming raid from: " <> nm
                        cmds.publish "fig monitor twitch raid" $ encodeUtf8 nm
                      _else -> log "Failed to extract user from raid event"
                  Just ("channel.follow" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          event .: "user_login"
                    case Aeson.parseMaybe parseEvent res of
                      Just nm -> do
                        log $ "New follower: " <> nm
                        cmds.publish "fig monitor twitch follow" $ encodeUtf8 nm
                      _else -> log "Failed to extract user from follow event"
                  Just ("channel.subscribe" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          login <- event .: "user_login"
                          gift <- event .: "is_gift"
                          pure (login, gift)
                    case Aeson.parseMaybe parseEvent res of
                      Just (nm, False) -> do
                        log $ "New subscriber: " <> nm
                        cmds.publish "fig monitor twitch subscribe" $ encodeUtf8 nm
                      Just _ -> log "Skipping gifted subscription"
                      _else -> log "Failed to extract user from subscribe event"
                  Just ("channel.cheer" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          nm <- event .: "user_login"
                          bits <- event .: "bits"
                          pure (nm, bits)
                    case Aeson.parseMaybe parseEvent res of
                      Just (nm, bits) -> do
                        log $ "New cheer: " <> nm <> " " <> tshow bits
                        cmds.publish "fig monitor twitch cheer"
                          . encodeUtf8 . Text.unwords $
                          [nm, bits]
                      _else -> log "Failed to extract user from cheer event"
                  Just ("channel.subscription.gift" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          nm <- event .: "user_login"
                          num <- event .: "total"
                          pure (nm, num)
                    case Aeson.parseMaybe parseEvent res of
                      Just (nm, num) -> do
                        log $ "User " <> nm <> " gifted subs: " <> tshow num
                        cmds.publish "fig monitor twitch gift"
                          . encodeUtf8 . Text.unwords $
                          [nm, num]
                      _else -> log "Failed to extract user from gift sub event"
                  Just ("channel.poll.begin" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          event .: "id"
                    case Aeson.parseMaybe parseEvent res of
                      Just pollid -> do
                        log $ "Poll begin: " <> pollid
                        cmds.publish "fig monitor twitch poll begin" $ encodeUtf8 pollid
                      _else -> log "Failed to extract ID from payload for poll begin event"
                  Just ("channel.poll.end" :: Text) -> do
                    let parseEvent o = do
                          payload <- o .: "payload"
                          event <- payload .: "event"
                          status <- event .: "status"
                          pollid <- event .: "id"
                          event .: "choices" >>= \case
                            Aeson.Array cs -> do
                              choices <- forM cs \case
                                Aeson.Object c -> do
                                  t <- c .: "title"
                                  v <- c .: "votes"
                                  pure (t, v)
                                _else -> mempty
                              pure (status, pollid, toList choices)
                            _else -> mempty
                    case Aeson.parseEither parseEvent res of
                      Right (status :: Text, pollid, choices :: [(Text, Integer)]) -> do
                        when (status /= "archived") do
                          let schoices = (\(t, v) -> t <> "\t" <> tshow v) <$> choices
                          log $ "Poll end: " <> pollid
                          cmds.publish "fig monitor twitch poll end" . encodeUtf8 . Text.intercalate "\n"
                           $ [pollid] <> schoices
                      Left err -> log $ "Failed to extract ID from payload for poll end event: " <> pack err
                  _else -> log $ "Received unknown notification event: " <> tshow resp
                Just "session_keepalive" -> pure ()
                _else -> log $ "Received unknown response: " <> tshow resp
      )
      (\_cmds ev d -> do
          let args = Text.splitOn "\t" $ decodeUtf8 d
          case (ev, args) of
            ("fig monitor twitch poll create", [title, schoices]) -> do
              let choices = Text.splitOn "\n" schoices
              runAuthed cfg do
                user <- loginToUserId cfg.userLogin
                poll title choices user
            ("fig monitor twitch prediction create", [title, schoices]) -> do
              let choices = Text.splitOn "\n" schoices
              runAuthed cfg do
                user <- loginToUserId cfg.userLogin
                createPrediction title choices user
            ("fig monitor twitch prediction finish", [pid, oid]) -> do
              runAuthed cfg do
                user <- loginToUserId cfg.userLogin
                finishPrediction pid oid user
            ("fig monitor twitch vip add", [u]) -> do
              runAuthed cfg do
                user <- loginToUserId cfg.userLogin
                loginToMaybeUserId u >>= \case
                  Nothing -> pure ()
                  Just vipuser -> addVIP vipuser user
            ("fig monitor twitch vip remove", [u]) -> do
              runAuthed cfg do
                user <- loginToUserId cfg.userLogin
                loginToMaybeUserId u >>= \case
                  Nothing -> pure ()
                  Just vipuser -> removeVIP vipuser user
            ("fig monitor twitch vip shoutout", [u]) -> do
              runAuthed cfg do
                user <- loginToUserId cfg.userLogin
                loginToMaybeUserId u >>= \case
                  Nothing -> pure ()
                  Just souser -> shoutout souser user
            _else -> log $ "Invalid incoming message: " <> tshow (ev, args)
      )
      (pure ())

twitchChannelLiveMonitor :: Config -> (Text, Text) -> IO ()
twitchChannelLiveMonitor cfg busAddr = do
  busClient busAddr
    (\cmds -> do
        let
          loop :: IO ()
          loop = do
            log "Updating liveness..."
            live <- runAuthed cfg $ usersAreLive cfg.monitor
            if null live
              then log "Update complete! No users live"
              else log $ "Update complete! Live users: " <> Text.unwords (Set.toList live)
            cmds.publish "fig monitor twitch stream online" . encodeUtf8 . Text.unwords $ Set.toList live
            threadDelay $ 5 * 60 * 1000000 -- wait 5 minutes
            loop
        loop
    )
    (\_cmds _ev _d -> pure ())
    (pure ())

data IRCMessage = IRCMessage
  { tags :: !(Map.Map Text Text)
  , prefix :: !(Maybe Text)
  , command :: !Text
  , params :: ![Text]
  } deriving (Show, Eq, Ord)

parseIRCMessage :: Text -> IRCMessage
parseIRCMessage (Text.strip -> fullrest) =
  let
    (tags, tagsrest) =
      if Text.head fullrest == '@'
      then
        let (tstr, rest) = Text.breakOn " " fullrest
        in ( Map.fromList $ second (Text.drop 1) . Text.breakOn "=" <$> Text.splitOn ";" (Text.drop 1 tstr)
           , Text.strip rest
           )
      else (Map.empty, fullrest)
    (prefix, prefixrest) =
      if Text.head tagsrest == ':'
      then
        let (pstr, rest) = Text.breakOn " " tagsrest
        in ( Just $ Text.drop 1 pstr
           , Text.strip rest
           )
      else (Nothing, tagsrest)
    (command, cmdrest) = Text.breakOn " " prefixrest
    params = case Text.breakOn ":" $ Text.strip cmdrest of
      (Text.strip -> "", rest) -> [rest]
      (ps, rest) -> Text.splitOn " " (Text.strip ps) <> [Text.drop 1 rest]
  in IRCMessage{..}

twitchChatClient :: Config -> (Text, Text) -> IO ()
twitchChatClient cfg busAddr = do
  log "Starting chatbot"
  case headMay cfg.monitor of
    Nothing -> pure ()
    Just chan -> WS.runSecureClient "irc-ws.chat.twitch.tv" 443 "/" \conn -> do
      WS.sendTextData conn $ "PASS oauth:" <> cfg.userToken
      WS.sendTextData conn ("NICK lcolonq" :: Text)
      WS.sendTextData conn ("CAP REQ :twitch.tv/commands twitch.tv/tags" :: Text)
      WS.sendTextData conn $ "JOIN #" <> chan
      -- WS.sendTextData conn ("PRIVMSG #lcolonq :test the other direction" :: Text)
      busClient busAddr
        (\cmds -> do
            cmds.subscribe "fig monitor twitch chat outgoing"
            forever do
              resp <- WS.receiveData conn
              forM (Text.lines resp) $ \line -> do
                let msg = parseIRCMessage line
                case msg.command of
                  "PING" -> do
                    log "Received PING, sending PONG"
                    WS.sendTextData conn $ "PONG :" <> mconcat msg.params
                  "CLEARCHAT" -> do
                    log "Received CLEARCHAT"
                    cmds.publish "fig monitor twitch chat clear-chat" . encodeUtf8 $ Text.unwords msg.params
                  "NOTICE" -> do
                    log "Received NOTICE"
                    cmds.publish "fig monitor twitch chat notice" . encodeUtf8 $ Text.unwords msg.params
                  "USERNOTICE" -> do
                    log "Received USERNOTICE"
                    cmds.publish "fig monitor twitch chat user-notice" . encodeUtf8 $ Text.unwords msg.params
                  "PRIVMSG"
                    | Just displaynm <- Map.lookup "display-name" msg.tags
                    , Nothing <- Map.lookup "custom-reward-id" msg.tags -> do
                        log $ "Received chat message from: " <> displaynm
                        cmds.publish "fig monitor twitch chat incoming" . encodeUtf8 . Text.unwords $
                          [ displaynm
                          , Text.intercalate "\n" $ (\(key, v) -> key <> "\t" <> v) <$> Map.toList msg.tags
                          ] <> drop 1 msg.params
                  _ -> pure ()
        )
        (\_cmds ev d -> do
            case ev of
              "fig monitor twitch chat outgoing" -> do
                let msg = decodeUtf8 d
                log $ "Sending chat message: " <> msg
                WS.sendTextData conn $ mconcat
                  [ "PRIVMSG #"
                  , chan
                  , " :"
                  , msg
                  ]
              _else -> log $ "Invalid incoming event: " <> tshow ev
        )
        (pure ())

userTokenRedirectServer :: Config -> Bool -> IO ()
userTokenRedirectServer cfg rw = do
  log "Starting token redirect server on port 4444"
  Scotty.scottyOpts opts do
    Scotty.get "/" do
      Scotty.html $ mconcat
        [ "<a href=\"https://id.twitch.tv/oauth2/authorize?response_type=token"
        , "&client_id=", Text.Lazy.fromStrict cfg.clientId
        , "&redirect_uri=http://localhost:4444"
        , "&scope=", Text.Lazy.replace ":" "%3A" $ Text.Lazy.intercalate "+" scopes
        , "\">Authenticate</a>"
        ]
  where
    opts = Scotty.Options
      { Scotty.verbose = 0
      , Scotty.settings = setPort 4444 (Scotty.settings def)
      }
    scopes = if rw then scopesReadWrite else scopesReadOnly
    scopesReadWrite =
      [ "channel:manage:polls"
      , "channel:manage:predictions"
      , "channel:manage:redemptions"
      , "channel:manage:vips"
      , "channel:read:polls"
      , "channel:read:predictions"
      , "channel:read:redemptions"
      , "channel:read:subscriptions"
      , "channel:read:vips"
      , "channel:moderate"
      , "moderator:read:followers"
      , "moderator:read:chatters"
      , "moderator:manage:shoutouts"
      , "chat:edit"
      , "chat:read"
      , "bits:read"
      ]
    scopesReadOnly =
      [
      ]

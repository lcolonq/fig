module Fig.Monitor.Twitch.Chatbot
  ( twitchChatbot
  ) where

import Fig.Prelude

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import qualified Wuss as WS
import qualified Network.WebSockets.Connection as WS

import Fig.Bus.Binary.Client
import Fig.Monitor.Twitch.Utils

twitchChatbot :: Config -> (Text, Text) -> IO ()
twitchChatbot cfg busAddr = do
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
                    , Just pfx <- msg.prefix
                    , Nothing <- Map.lookup "custom-reward-id" msg.tags -> do
                        log $ "Received chat message from: " <> displaynm
                        cmds.publish "fig monitor twitch chat incoming" . encodeUtf8 . Text.unwords $
                          [ displaynm, "\t", fst $ Text.breakOn "!" pfx
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

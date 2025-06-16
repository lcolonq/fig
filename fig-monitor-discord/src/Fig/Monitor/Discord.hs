{-# Language QuasiQuotes #-}

module Fig.Monitor.Discord where

import Fig.Prelude

import Control.Monad.Reader (runReaderT)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as BS.Base64
import qualified Data.Map.Strict as Map

import qualified Text.Regex.PCRE.Heavy as PCRE

import qualified Discord as Dis
import qualified Discord.Types as Dis
import qualified Discord.Requests as Dis
import qualified Discord.Interactions as Dis

import Fig.Utils.SExpr
import Fig.Bus.SExpr.Client
import Fig.Monitor.Discord.Utils

data OutgoingMessage = OutgoingMessage
  { chan :: !Integer
  , user :: !Text
  , msg :: !Text
  }

data EmojiInfo = EmojiInfo
  { id :: !Dis.EmojiId
  , animated :: !Bool
  } deriving Show

data BotInfo = BotInfo
  { userId :: !Dis.UserId
  , emotes :: !(Map.Map Text EmojiInfo)
  }

stickerUrl :: Text -> Dis.StickerFormatType -> Text
stickerUrl sid ty = base <> sid <> "." <> ext
  where
    base = "https://media.discordapp.net/stickers/"
    ext = case ty of
      Dis.StickerFormatTypeAPNG -> "png"
      Dis.StickerFormatTypeLOTTIE -> "png"
      Dis.StickerFormatTypePNG -> "png"
      Dis.StickerFormatTypeGIF -> "gif"

discordBot :: Config -> (Text, Text) -> IO ()
discordBot cfg busAddr = do
  outgoing <- Chan.newChan @OutgoingMessage
  botInfo <- MVar.newEmptyMVar
  busClient busAddr
    (\cmds -> do
        cmds.subscribe [sexp|(monitor discord chat outgoing)|]
        err <- Dis.runDiscord Dis.def
          { Dis.discordToken = cfg.authToken
          , Dis.discordOnStart = do
              let activity = Dis.mkActivity "LCOLONQ" Dis.ActivityTypeCompeting
              let opts = Dis.UpdateStatusOpts
                    { updateStatusOptsSince = Nothing
                    , updateStatusOptsActivities = [activity]
                    , updateStatusOptsNewStatus = Dis.UpdateStatusOnline
                    , updateStatusOptsAFK = False
                    }
              Dis.sendCommand (Dis.UpdateStatus opts)
              userId <- Dis.restCall Dis.GetCurrentUser >>= \case
                Left e -> throwM . FigMonitorDiscordException $ "Failed to retrieve discord user: " <> tshow e
                Right u -> pure $ Dis.userId u
              emotes <- Map.unions <$> forM cfg.guildIds \guildId -> do
                let gid = Dis.DiscordId $ Dis.Snowflake $ fromIntegral guildId
                Dis.restCall (Dis.ListGuildEmojis gid) >>= \case
                  Left e -> throwM . FigMonitorDiscordException $ "Failed to retrieve server emoji: " <> tshow e
                  Right emotes -> pure . Map.fromList
                    $ Maybe.mapMaybe
                    (\e -> do
                        eid <- Dis.emojiId e
                        animated <- Dis.emojiAnimated e
                        pure (Dis.emojiName e, EmojiInfo { id = eid, animated })
                    )
                    emotes
              log $ tshow emotes
              liftIO . MVar.putMVar botInfo $ BotInfo{..}
              log "Initialized Discord bot"
              dst <- ask
              liftIO . void . Async.async . forever $ flip runReaderT dst do
                o <- liftIO $ Chan.readChan outgoing
                let cid = Dis.DiscordId $ Dis.Snowflake $ fromIntegral o.chan
                void . Dis.restCall . Dis.CreateMessage cid $ mconcat
                  [ "`<", o.user, ">` "
                  , o.msg
                  ]
          , Dis.discordOnLog = log
          , Dis.discordOnEvent = \case
              Dis.Ready _ _ _ _ _ _ (Dis.PartialApplication i _) -> do
                cmd <- case Dis.createUser "ping" of
                  Nothing -> throwM $ FigMonitorDiscordException "Failed to create ping command"
                  Just cmd -> pure cmd
                log "Creating application command"
                resp <- Dis.restCall $ Dis.CreateGlobalApplicationCommand i cmd
                log $ tshow resp
              Dis.InteractionCreate cmd@Dis.InteractionApplicationCommand{}  -> do
                void . Dis.restCall . Dis.CreateInteractionResponse (Dis.interactionId cmd) (Dis.interactionToken cmd) $ Dis.interactionResponseBasic "pong"
              Dis.MessageCreate m -> do
                binfo <- liftIO $ MVar.readMVar botInfo
                let
                  chan = Dis.messageChannelId m
                  auth = Dis.messageAuthor m
                  mmemb = Dis.messageMember m
                  msticker = Dis.messageStickerItems m >>= headMay
                  name = fromMaybe (Dis.userName auth) (Dis.memberNick =<< mmemb)
                  attach = Dis.attachmentProxy <$> Dis.messageAttachments m
                  reply = Dis.messageReferencedMessage m
                  mentions = Map.fromList
                    . filter (isJust . snd)
                    $ (\u -> (Dis.userId u, Dis.memberNick <$> Dis.userMember u))
                    <$> Dis.messageMentions m
                  replyNick = join . join $ flip Map.lookup mentions . Dis.userId . Dis.messageAuthor =<< reply
                  replyUser = if isJust replyNick then replyNick else Dis.userName . Dis.messageAuthor <$> reply
                  msg = Dis.messageContent m
                  replyMsg = Dis.messageContent <$> reply
                  replyStr = replyUser >>= \ru ->
                    if ru == "The Computer"
                    then replyMsg >>= (
                      PCRE.scan [PCRE.re|^`\<(.*)\>`|]
                          >>> \case
                              ((_, [compName]):_) -> Just compName
                              _ -> Just ru
                    )
                    else Just ru
                  msgReplacedEmotes = PCRE.gsub
                    [PCRE.re|<a?:([\w_-]+):(\d+)>|]
                    (\(_ :: Text) -> \case
                      ([emotename, _num] :: [Text]) -> case emotename of
                        "mrgreen" -> "🟢"
                        "mrred" -> "🔴"
                        "mrblue" -> "🔵"
                        "mryellow" -> "🟡"
                        _ -> ":" <> emotename <> ":"
                      _ -> "<unknown emote>"
                    )
                    msg
                  processedMsg = case msticker of
                    Just sticker ->
                      (case Dis.stickerItemName sticker of
                         "Eval Apply" -> "☯︎"
                         snm -> snm
                      ) <> " (" <> stickerUrl (tshow . Dis.unId $ Dis.stickerItemId sticker) (Dis.stickerItemFormatType sticker) <> ")"
                    _ -> msgReplacedEmotes
                -- in unless (Dis.userIsBot auth) do
                unless (Dis.userId auth == binfo.userId) do
                  log $ "Received: " <> processedMsg <> " (from " <> name <> ")"
                  liftIO $ cmds.publish [sexp|(monitor discord chat incoming)|]
                    [ SExprInteger . fromIntegral . Dis.unSnowflake $ Dis.unId chan
                    , SExprString . BS.Base64.encodeBase64 $ encodeUtf8 name
                    , SExprList []
                    , SExprString . BS.Base64.encodeBase64 . encodeUtf8 . Text.strip . Text.intercalate " "
                      $ maybe [] ((:[]) . (<>":")) replyStr <>
                        mconcat
                        [ [processedMsg]
                        , attach
                        ]
                    ]
              _ -> pure ()
          }
        log err
    )
    (\_cmds d -> do
        case d of
          SExprList [ev, SExprInteger chan, SExprString euser, SExprString emsg]
            | ev == [sexp|(monitor discord chat outgoing)|]
            , Right user <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 euser)
            , Right msg <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 emsg) -> do
                log $ "Sending: " <> msg <> " (from " <> user <> ")"
                binfo <- liftIO $ MVar.readMVar botInfo
                let
                  replacements :: [(Text, Text)] =
                    [ ("🟢", ":mrgreen:")
                    , ("🔵", ":mrblue:")
                    , ("🔴", ":mrred:")
                    , ("🟡", ":mryellow:")
                    ]
                  newMsg = foldr' (\(n, r) h -> Text.replace n r h) msg replacements
                  msgReplacedEmotes = PCRE.gsub
                    [PCRE.re|:([\w_-]+):|]
                    (\(_ :: Text) -> \case
                      ([emotename] :: [Text]) -> case Map.lookup emotename binfo.emotes of
                        Just einfo ->
                          "<" <> (if einfo.animated then "a" else "")
                          <> ":" <> emotename <> ":" <> tshow einfo.id <> ">"
                        _ -> ":" <> emotename <> ":"
                      _ -> "<unknown emote>"
                    )
                    newMsg
                Chan.writeChan outgoing OutgoingMessage { chan, user, msg = msgReplacedEmotes }
          _ -> log $ "Invalid outgoing message: " <> tshow d
    )
    (pure ())

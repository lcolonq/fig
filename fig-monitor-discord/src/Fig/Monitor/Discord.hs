{-# Language QuasiQuotes #-}

module Fig.Monitor.Discord where

import Fig.Prelude

import Control.Monad (unless)
import Control.Monad.Reader (runReaderT)
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan

import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as BS.Base64
import qualified Data.Map.Strict as Map

import qualified Text.Regex.PCRE.Heavy as PCRE

import qualified Discord as Dis
import qualified Discord.Types as Dis
import qualified Discord.Requests as Dis
import qualified Discord.Interactions as Dis

import Fig.Utils.SExpr
import Fig.Bus.Client
import Fig.Monitor.Discord.Utils

data OutgoingMessage = OutgoingMessage
  { chan :: Integer
  , user :: Text
  , msg :: Text
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
              Dis.MessageCreate m ->
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
                      PCRE.scan [PCRE.re|^`\<(.*)\>`|] >>> \case
                                    ((_, [compName]):_) -> Just compName
                                    _ -> Just ru
                    )
                    else Just ru
                  msgReplacedEmotes = PCRE.gsub
                    [PCRE.re|<:([\w_-]+):(\d+)>|]
                    (\(_ :: Text) -> \case
                      ([emotename, _num] :: [Text]) -> case emotename of
                        "mrgreen" -> "🟢"
                        "mrred" -> "🔴"
                        "mrblue" -> "🔵"
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
                in unless (Dis.userIsBot auth) do
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
                let replacements :: [(Text, Text)] =
                      [ (":mrgreen:", "<:mrgreen:1093634800792911972>")
                      , (":mrblue:", "<:mrblue:1154526193358491719>")
                      , (":mrred:", "<:mrred:1154524307649724449>")
                      ]
                let newMsg = foldr' (\(n, r) h -> Text.replace n r h) msg replacements
                Chan.writeChan outgoing OutgoingMessage { chan, user, msg = newMsg }
          _ -> log $ "Invalid outgoing message: " <> tshow d
    )
    (pure ())

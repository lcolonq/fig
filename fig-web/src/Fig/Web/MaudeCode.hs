{-# Language QuasiQuotes #-}

module Fig.Web.MaudeCode
  ( server
  ) where

import Fig.Prelude

import Prelude (round)

import System.Timeout (timeout)

import qualified Control.Concurrent.MVar as MVar

import qualified Data.ByteString.Base64 as BS.Base64
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Time.Clock.POSIX as Time

import qualified Network.Wai as Wai
-- import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Scotty as Sc

import Fig.Utils.SExpr
import Fig.Bus.SExpr.Client
import Fig.Web.Utils

newtype RequestChatCompletions = RequestChatCompletions
  { message :: Text
  } deriving (Show, Eq, Ord, Generic)
instance Aeson.FromJSON RequestChatCompletions where
  parseJSON = Aeson.withObject "RequestChatCompletions" \o -> do
    messages :: [Aeson.Object] <- o Aeson..: "messages"
    message :: Text <- Text.replace "\n" " " . Text.unwords <$> forM messages (Aeson..: "content")
    pure RequestChatCompletions{..}

data Globals = Globals
  { responses :: !(MVar.MVar (Map.Map Integer (MVar.MVar Text)))
  , nextIndex :: !(MVar.MVar Integer)
  }

newGlobals :: IO Globals
newGlobals = do
  responses <- MVar.newMVar Map.empty
  nextIndex <- MVar.newMVar 0
  pure Globals{..}

server :: Config -> (Text, Text) -> IO ()
server cfg busAddr = do
  log $ "Web server starting on port " <> tshow cfg.port <> "..."
  g <- newGlobals
  busClient busAddr
    (\cmds -> do
        log "Connected to bus!"
        cmds.subscribe [sexp|(monitor irc chat incoming)|]
        Warp.run cfg.port =<< app g cmds
    )
    (\_cmds d -> do
        case d of
          SExprList [ev, SExprString chan, SExprString euser, _, SExprString emsg]
            | ev == [sexp|(monitor irc chat incoming)|]
            , chan == "#maudecode"
            , Right user <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 euser)
            , user `elem` (["llll", "maude"] :: [Text])
            , Right msg <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 emsg) -> do
                log $ "Maude Code response from " <> user <> ": " <> msg
                responses <- MVar.readMVar g.responses
                log $ "Active responses: " <> tshow (Map.keys responses)
                next <- MVar.readMVar g.nextIndex
                let (pre, _post) = Text.breakOn ":" msg
                let idx = case readMaybe (unpack pre) :: Maybe Integer of
                            Nothing -> next - 1
                            Just i -> i
                case Map.lookup idx responses of
                  Nothing -> log $ "Responding to unknown message: " <> tshow idx
                  Just mv -> MVar.putMVar mv msg
                pure ()
          _ -> log $ "Invalid message: " <> tshow d
    )
    (pure ())

app :: Globals -> Commands IO -> IO Wai.Application
app g cmds = do
  log "Connected! Server active."
  let modelinfo = Aeson.object
        [ ( "id", Aeson.toJSON @Text "maude-9" )
        , ( "object", Aeson.toJSON @Text "model" )
        , ( "created", Aeson.toJSON @Integer 894945600 )
        , ( "owned_by", Aeson.toJSON @Text "modclonk" )
        ]
  Sc.scottyApp do
    onGet "/" $ respondHTML do
      head_ do
        title_ "Maude Code by MODCLONK | Human Person, Cool, Neat"
        style_ $ Text.unlines
          [ "body { overflow: hidden; margin: 0; padding: 0; height: 100%; width: 100%; background-color: #0f1011; color: #eae6d6; text-align: center; }"
          , "h1 { font: 4rem serif; font-weight: bold; }"
          , "p { font: 1.5rem sans-serif; color: #9a9184; width: 50%; margin: auto; }"
          ]
      body_ do
        div_ [id_ "content"] do
          h1_ "Maude Code"
          p_ "Maude Code is a human being who reads your messages, eats snacks, and plays a videogame about horse women on her mobile phone and desktop computer. Talk, converse, and chat with natural language."
    onGet "/v1/models" do
      respondJSON $ Aeson.object
        [ ( "object", Aeson.toJSON @Text "list" )
        , ( "choices", Aeson.toJSON @[Aeson.Value] [modelinfo] )
        ]
    onGet "/v1/models/maude-9" $ respondJSON modelinfo
    onPost "/v1/chat/completions" do
      b :: RequestChatCompletions <- bodyJSON
      log $ "Responding to completion request: " <> b.message
      index <- liftIO $ MVar.modifyMVar g.nextIndex $ \old -> pure (old + 1, old)
      liftIO $ cmds.publish [sexp|(monitor irc chat outgoing)|]
        [ SExprString "#maudecode"
        , SExprString $ BS.Base64.encodeBase64 $ encodeUtf8 $ "maude " <> tshow index
        , SExprString $ BS.Base64.encodeBase64 $ encodeUtf8 b.message
        ]
      let tokens :: Int = 1000
      responder <- liftIO MVar.newEmptyMVar
      liftIO $ MVar.modifyMVar_ g.responses $ pure . Map.insert index responder
      response <- liftIO $ timeout 60_000_000 $ MVar.takeMVar responder
      liftIO $ MVar.modifyMVar_ g.responses $ pure . Map.delete index
      uuid <- liftIO UUID.nextRandom
      timestamp :: Integer <- round <$> liftIO Time.getPOSIXTime
      respondJSON $ Aeson.object
        [ ( "id", Aeson.toJSON $ UUID.toText uuid )
        , ( "object", Aeson.toJSON @Text "chat.completion" )
        , ( "created", Aeson.toJSON timestamp )
        , ( "model", Aeson.toJSON @Text "maude-9" )
        , ( "choices", Aeson.toJSON @[Aeson.Value]
            [ Aeson.object
              [ ( "index", Aeson.toJSON @Int 0)
              , ( "message", Aeson.object
                  [ ( "role", Aeson.toJSON @Text "assistant" )
                  , ( "content", Aeson.toJSON $ fromMaybe "maude is sleepy" response )
                  , ( "refusal", Aeson.toJSON $ Nothing @() )
                  , ( "annotations", Aeson.toJSON @[()] [] )
                  ]
                )
              , ( "logprobs", Aeson.toJSON $ Nothing @() )
              , ( "finish_reason", Aeson.toJSON @Text "stop" )
              ]
            ]
          )
        , ( "usage", Aeson.toJSON @[Aeson.Value]
            [ Aeson.object
              [ ( "prompt_tokens", Aeson.toJSON tokens )
              , ( "completion_tokens", Aeson.toJSON tokens )
              , ( "total_tokens", Aeson.toJSON $ tokens + tokens )
              , ( "prompt_tokens_details", Aeson.object
                  [ ( "cached_tokens", Aeson.toJSON @Int 0 )
                  , ( "audio_tokens", Aeson.toJSON @Int 0 )
                  ]
                )
              , ( "completion_tokens_details", Aeson.object
                  [ ( "reasoning_tokens", Aeson.toJSON @Int 0 )
                  , ( "audio_tokens", Aeson.toJSON @Int 0 )
                  , ( "accepted_prediction_tokens", Aeson.toJSON @Int 0 )
                  , ( "rejected_prediction_tokens", Aeson.toJSON @Int 0 )
                  ]
                )
              ]
            ]
          )
        , ( "service_tier", Aeson.toJSON @Text "default" )
        ]
    Sc.notFound do
      respondText "not found\n"

{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Fig.Web.Utils
  ( FigWebException(..)
  , loadConfig
  , Config(..)
  , resetUserPassword
  , module Network.HTTP.Types.Status
  , onGet, onPost, onPut, onDelete
  , status
  , queryParam, queryParamMaybe, formParam, formParamMaybe, pathParam
  , header
  , respondText, respondJSON, respondHTML
  , Credentials(..)
  , authed
  , WebsocketHandler
  , websocket
  , BusEventHandler, BusEventHandlers
  , busEvents
  , handleBusEvent
  , subscribeBusEvents
  ) where

import Fig.Prelude

import System.Exit (ExitCode(..))
import qualified System.Process as Proc

import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.L
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map

import Network.HTTP.Types.Status
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets as WS

import qualified Web.Scotty as Sc

import qualified Toml

import Fig.Bus.Binary.Client

newtype FigWebException = FigWebException Text
  deriving (Show, Eq, Ord)
instance Exception FigWebException

data Config = Config
  { port :: !Int
  , assetPath :: !FilePath
  , clientId :: !Text
  , authToken :: !Text
  , dbHost :: !Text
  , lldapCli :: !FilePath
  , lldapHost :: !Text
  , lldapUser :: !Text
  , lldapPassword :: !Text
  } deriving (Show, Eq, Ord)

configCodec :: Toml.TomlCodec Config
configCodec = do
  port <- Toml.int "port" Toml..= (\a -> a.port)
  assetPath <- Toml.string "asset_path" Toml..= (\a -> a.assetPath)
  clientId <- Toml.text "client_id" Toml..= (\a -> a.clientId)
  authToken <- Toml.text "auth_token" Toml..= (\a -> a.authToken)
  dbHost <- Toml.text "db_host" Toml..= (\a -> a.dbHost)
  lldapCli <- Toml.string "lldap_cli" Toml..= (\a -> a.lldapCli)
  lldapHost <- Toml.text "lldap_host" Toml..= (\a -> a.lldapHost)
  lldapUser <- Toml.text "lldap_user" Toml..= (\a -> a.lldapUser)
  lldapPassword <- Toml.text "lldap_password" Toml..= (\a -> a.lldapPassword)
  pure $ Config{..}

loadConfig :: FilePath -> IO Config
loadConfig path = Toml.decodeFileEither configCodec path >>= \case
  Left err -> throwM . FigWebException $ tshow err
  Right config -> pure config

-- | Reset the password in LDAP for the specified user (creating the user if necessary)
resetUserPassword :: MonadIO m => Config -> Text -> Text -> m (Maybe Text)
resetUserPassword cfg user uid = do
  let login = Text.toLower user
  password <- UUID.toText <$> liftIO UUID.nextRandom
  (exitCode, out0, err0) <- liftIO . flip Proc.readCreateProcessWithExitCode ""
    . Proc.proc cfg.lldapCli $ unpack <$>
    [ "-H", cfg.lldapHost
    , "-D", cfg.lldapUser
    , "-w", cfg.lldapPassword
    , "user", "add", login, uid <> "@users.colonq.computer"
    , "-p", password
    , "-f", uid
    ]
  (_, out1, err1) <- liftIO . flip Proc.readCreateProcessWithExitCode ""
    . Proc.proc cfg.lldapCli $ unpack <$>
    [ "-H", cfg.lldapHost
    , "-D", cfg.lldapUser
    , "-w", cfg.lldapPassword
    , "user", "group", "add", login, "fig_users"
    ]
  case exitCode of
    ExitSuccess -> pure $ Just password
    ExitFailure _ -> do
      log . pack $ mconcat
        [ "LDAP CLI error:\n"
        , out0, err0
        , out1, err1
        ]
      pure Nothing

onGet :: Sc.RoutePattern -> Sc.ActionM () -> Sc.ScottyM ()
onGet = Sc.get

onPost :: Sc.RoutePattern -> Sc.ActionM () -> Sc.ScottyM ()
onPost = Sc.post

onPut :: Sc.RoutePattern -> Sc.ActionM () -> Sc.ScottyM ()
onPut = Sc.post

onDelete :: Sc.RoutePattern -> Sc.ActionM () -> Sc.ScottyM ()
onDelete = Sc.post

status :: Status -> Sc.ActionM ()
status = Sc.status

queryParam :: Sc.Parsable a => Text -> Sc.ActionM a
queryParam = Sc.queryParam . Text.L.fromStrict
queryParamMaybe :: Sc.Parsable a => Text -> Sc.ActionM (Maybe a)
queryParamMaybe = Sc.queryParamMaybe . Text.L.fromStrict

formParam :: Sc.Parsable a => Text -> Sc.ActionM a
formParam = Sc.formParam . Text.L.fromStrict
formParamMaybe :: Sc.Parsable a => Text -> Sc.ActionM (Maybe a)
formParamMaybe = Sc.formParamMaybe . Text.L.fromStrict

pathParam :: Sc.Parsable a => Text -> Sc.ActionM a
pathParam = Sc.pathParam . Text.L.fromStrict

header :: Text -> Sc.ActionM (Maybe Text)
header h = Sc.header (Text.L.fromStrict h) >>= \case
  Nothing -> pure Nothing
  Just t -> pure . Just $ Text.L.toStrict t

respondText :: Text -> Sc.ActionM ()
respondText = Sc.text . Text.L.fromStrict

respondJSON :: Aeson.ToJSON a => a -> Sc.ActionM ()
respondJSON = Sc.json

respondHTML :: Text -> Sc.ActionM ()
respondHTML = Sc.html . Text.L.fromStrict

data Credentials = Credentials
  { user :: Text
  , email :: Text
  }
authed :: (Credentials -> Sc.ActionM ()) -> Sc.ActionM ()
authed h = do
  muser <- header "Remote-User"
  memail <- header "Remote-Email"
  case (muser, memail) of
    (Just user, Just email) -> do
      let auth = Credentials{..}
      h auth
    _else -> do
      status status401
      respondText "you're not logged in buddy (this is probably a bug, go message clonk)"

type WebsocketHandler = (ByteString, WS.Connection -> IO ())
websocket :: [WebsocketHandler] -> Sc.ScottyM ()
websocket hs = Sc.middleware $ Wai.WS.websocketsOr WS.defaultConnectionOptions handler
  where
    handler pending = case lookup (WS.requestPath (WS.pendingRequest pending)) hs of
      Nothing -> WS.rejectRequest pending ""
      Just h -> WS.acceptRequest pending >>= \c -> WS.withPingThread c 30 (pure ()) $ h c

type BusEventHandler = (ByteString, ByteString -> IO ())
type BusEventHandlers = Map.Map ByteString (ByteString -> IO ())
busEvents :: [BusEventHandler] -> BusEventHandlers
busEvents = Map.fromList
handleBusEvent :: BusEventHandlers -> ByteString -> ByteString -> IO ()
handleBusEvent hs ev d = case Map.lookup ev hs of
  Just h -> h d
  Nothing -> log $ "Invalid event: " <> tshow ev
subscribeBusEvents :: Commands IO -> BusEventHandlers -> IO ()
subscribeBusEvents cmds hs = forM_ (Map.keys hs) $ \ev -> do
  cmds.subscribe ev

module Fig.Web.Module.Puzzle
  ( secure
  ) where

import Fig.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Fig.Web.Utils
import Fig.Web.Types
import Fig.Web.Auth
import Fig.Web.DB

data Puzzle = Puzzle
  { pid :: Text
  , name :: Text
  , author :: Text
  , authorid :: Text
  , body :: Text
  , checker :: Text
  } deriving (Show, Generic)
instance Aeson.ToJSON Puzzle

newPuzzlePid :: forall m. (MonadIO m, MonadCatch m) => m Text
newPuzzlePid = do
  uuid <- liftIO UUID.nextRandom
  pure $ UUID.toText uuid

puzzlePidKey :: Text -> ByteString
puzzlePidKey pid = "puzzle:" <> encodeUtf8 pid

puzzleKey :: Puzzle -> ByteString
puzzleKey p = puzzlePidKey p.pid

savePuzzle :: MonadIO m => DB -> Puzzle -> m ()
savePuzzle db p = do
  sadd db "puzzleids" [encodeUtf8 p.pid]
  hmset db (puzzleKey p)
    [ ("pid", encodeUtf8 p.pid)
    , ("name", encodeUtf8 p.name)
    , ("author", encodeUtf8 p.author)
    , ("authorid", encodeUtf8 p.authorid)
    , ("body", encodeUtf8 p.body)
    , ("checker", encodeUtf8 p.checker)
    ]

loadPuzzle :: MonadIO m => DB -> Text -> m (Maybe Puzzle)
loadPuzzle db p = do
  m <- hgetall db $ puzzlePidKey p
  pure $ do
    let field nm = decodeUtf8 <$> Map.lookup nm m
    pid <- field "pid"
    if pid == p
      then do
      name <- field "name"
      author <- field "author"
      authorid <- field "authorid"
      body <- field "body"
      checker <- field "checker"
      Just Puzzle {..}
      else Nothing

deletePuzzle :: MonadIO m => DB -> Text -> m ()
deletePuzzle db p = do
  srem db "puzzleids" [encodeUtf8 p]
  del db [puzzlePidKey p]

allPuzzles :: MonadIO m => DB -> m [Text]
allPuzzles db = smembers db "puzzleids" >>= \case
  Nothing -> pure []
  Just xs -> pure $ decodeUtf8 <$> xs

secure :: SecureModule
secure a = do
  onGet "/api/puzzle" do
    puzzles <- allPuzzles a.db
    respondJSON puzzles
  onGet "/api/puzzle/:pid" do
    pid <- pathParam "pid"
    loadPuzzle a.db pid >>= \case
      Nothing -> do
        status status404
        respondText "no such puzzle"
      Just p -> respondJSON p
  onDelete "/api/puzzle/:pid" do
    pid <- pathParam "pid"
    deletePuzzle a.db pid
  onPost "/api/puzzle" $ authed a \creds -> do
    log "Creating puzzle"
    let author = creds.user
    let authorid = creds.twitchId
    pid <- newPuzzlePid
    log $ "pid: " <> tshow pid
    name <- formParam "name"
    body <- formParam "body"
    checker <- formParam "checker"
    savePuzzle a.db Puzzle {..}
    respondText pid

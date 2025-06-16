module Fig.Web.Module.Puzzle
  ( secure
  ) where

import Fig.Prelude

import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import qualified Web.Scotty as Sc
import qualified Lucid as L
import Text.HTML.SanitizeXSS (sanitize)

import Fig.Utils.FFI (checkAnswer)
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

allPuzzlePids :: MonadIO m => DB -> m [Text]
allPuzzlePids db = smembers db "puzzleids" >>= \case
  Nothing -> pure []
  Just xs -> pure $ decodeUtf8 <$> xs

allPuzzles :: MonadIO m => DB -> m [Puzzle]
allPuzzles db = do
  pids <- allPuzzlePids db
  catMaybes <$> forM pids \pid -> do
    loadPuzzle db pid

puzzleUrl :: Puzzle -> Text
puzzleUrl p = "/2d2p/" <> p.pid

authorUrl :: Text -> Text
authorUrl a = "/2d2p/author/" <> a

puzzleNameAuthorLinks :: Puzzle -> L.Html ()
puzzleNameAuthorLinks p = do
  a_ [href_ $ puzzleUrl p] $ L.toHtml p.name
  " by "
  a_ [href_ $ authorUrl p.author] $ L.toHtml p.author

page :: L.Html () -> Sc.ActionM ()
page b = do
  respondHTML do
    head_ do
      title_ "today: to puzzle!"
    body_ do
      h1_ "today: to puzzle!"
      b

secure :: SecureModule
secure a = do
  onGet "/2d2p" $ authed a \creds -> do
    puzzles <- allPuzzles a.db
    page do
      h2_ "all puzzles"
      ul_ $ forM_ puzzles \p -> do
        li_ do
          puzzleNameAuthorLinks p
          when (creds.twitchId == p.authorid) $ input_
            [ type_ "button"
            , value_ "delete"
            , onclick_ $ mconcat
              [ "fetch('", puzzleUrl p
              , "', { method: 'DELETE' }).then(() => window.location.reload());"
              ]
            ]
      h2_ "create new puzzle"
      form_ [method_ "post"] do
        label_ "name: "
        input_ [type_ "text", name_ "name"]
        br_ []
        label_ "body: "
        textarea_ [name_ "body"] $ pure ()
        br_ []
        label_ "checker: "
        textarea_ [name_ "checker"] $ pure ()
        br_ []
        input_ [type_ "submit", value_ "submit"]
  onPost "/2d2p" $ authed a \creds -> do
    let author = creds.user
    let authorid = creds.twitchId
    pid <- newPuzzlePid
    log $ "Creating new puzzle with PID: " <> tshow pid
    name <- formParam "name"
    body <- formParam "body"
    checker <- formParam "checker"
    let p = Puzzle {..}
    savePuzzle a.db p
    redirect $ puzzleUrl p
  onGet "/2d2p/:pid" do
    pid <- pathParam "pid"
    loadPuzzle a.db pid >>= \case
      Nothing -> do
        status status404
        page $ h2_ "no such puzzle!"
      Just p -> page do
        h2_ $ puzzleNameAuthorLinks p
        p_ $ L.toHtmlRaw $ sanitize p.body
        form_ [method_ "post"] do
          input_ [type_ "text", name_ "answer"]
          input_ [type_ "submit", value_ "answer"]
  onPost "/2d2p/:pid" do
    pid <- pathParam "pid"
    answer <- formParam "answer"
    loadPuzzle a.db pid >>= \case
      Nothing -> do
        status status404
        page $ h2_ "no such puzzle!"
      Just p -> do
        checkAnswer p.checker answer >>= \case
          Left err -> do
            status status500
            page do
              h2_ "error running checker:"
              pre_ $ L.toHtml err
          Right res -> page do
            h2_ $ if res then "congratulations! that's right!" else "sorry, wrong answer!"
  onDelete "/2d2p/:pid" do
    pid <- pathParam "pid"
    log $ "Deleting puzzle with PID: " <> tshow pid
    deletePuzzle a.db pid

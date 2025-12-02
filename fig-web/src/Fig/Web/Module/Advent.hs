module Fig.Web.Module.Advent where

import Fig.Prelude

import qualified Lucid as L
import Text.HTML.SanitizeXSS (sanitize)

import Fig.Web.Utils
import Fig.Web.Types
import Fig.Web.Auth
import Fig.Web.DB as DB
import qualified Fig.Utils.FFI as FFI

keybase :: Integer -> Text -> Text -> ByteString
keybase pid part key = "advent:2025:" <> bspid <> ":" <> encodeUtf8 part <> ":" <> encodeUtf8 key
  where bspid = encodeUtf8 $ tshow pid

secure :: SecureModule
secure a = do
  onGet "/advent/puzzle/:pid" $ authed a \creds -> do
    pid :: Integer <- pathParam "pid"
    mpart1body <- fmap decodeUtf8 <$> DB.get a.db (keybase pid "part1" "body")
    part1solved <- DB.sismember a.db (keybase pid "part1" "solvers") (encodeUtf8 creds.twitchId)
    mpart2body <- fmap decodeUtf8 <$> DB.get a.db (keybase pid "part2" "body")
    part2solved <- DB.sismember a.db (keybase pid "part2" "solvers") (encodeUtf8 creds.twitchId)
    case (mpart1body, mpart2body) of
      (Just part1body, Just part2body) -> do
        respondHTML do
          head_ do
            title_ . L.toHtml $ "adventure of advent of code 2025: puzzle " <> tshow pid
            link_ [rel_ "icon", href_ "/assets/mrgreen.png"]
            link_ [rel_ "stylesheet", type_ "text/css", href_ "/main.css"]
          body_ [id_ "lcolonq-advent"] do
            div_ [class_ "lcolonq-advent-header"] do
              h1_ . L.toHtml $ "puzzle " <> tshow pid <> " (part 1)"
            div_ [class_ "lcolonq-advent-body"] do
              L.toHtmlRaw $ sanitize part1body
            a_ [class_ "lcolonq-advent-link", href_ $ "/advent/puzzle/" <> tshow pid <> "/part1/input.txt"]
              "click here to see your input"
            form_ [action_ $ "/advent/puzzle/" <> tshow pid <> "/part1/submit", method_ "post", class_ "lcolonq-advent-form"] do
              label_ [for_ "answer"] "Enter your answer:"
              input_ [type_ "text", name_ "answer", required_ ""]
              input_ [type_ "submit", class_ "lcolonq-advent-submit"]
            when part1solved do
              div_ [class_ "lcolonq-advent-congrats"] do
                "Congrats, you've solved Part 1! The puzzle continues below."
              div_ [class_ "lcolonq-advent-header"] do
                h1_ . L.toHtml $ "puzzle " <> tshow pid <> " (part 2)"
              div_ [class_ "lcolonq-advent-body"] do
                L.toHtmlRaw $ sanitize part2body
              a_ [class_ "lcolonq-advent-link", href_ $ "/advent/puzzle/" <> tshow pid <> "/part2/input.txt"]
                "click here to see your input"
              form_ [action_ $ "/advent/puzzle/" <> tshow pid <> "/part2/submit", method_ "post", class_ "lcolonq-advent-form"] do
                label_ [for_ "answer"] "Enter your answer:"
                input_ [type_ "text", name_ "answer", required_ ""]
                input_ [type_ "submit", class_ "lcolonq-advent-submit"]
              when part2solved $ div_ [class_ "lcolonq-advent-congrats"] do
                "Congrats, you've solved Part 2! The puzzle is complete!"
      _ -> do
        status status404
        respondText "puzzle not found. ask clonk about it, unless you're messing with the url. you url-messer-wither you."
  onGet "/advent/puzzle/:pid/:part/input.txt" $ authed a \creds -> do
    pid :: Integer <- pathParam "pid"
    part <- pathParam "part"
    let bstid = encodeUtf8 creds.twitchId
    DB.hget a.db (keybase pid part "inputs") bstid >>= \case
      Just inp ->
        respondText $ decodeUtf8 inp
      Nothing -> do
        DB.get a.db (keybase pid part "generator") >>= \case
          Nothing -> do
            status status404
            respondText "the puzzle has no generator associated with it. tell clonk please!"
          Just bsgen -> do
            let gen = decodeUtf8 bsgen
            FFI.genInputAnswer gen creds.twitchId >>= \case
              Left err -> do
                status status500
                respondText $ mconcat
                  [ "failed to generate an input for you. this is a problem! here's why:\n"
                  , err
                  ]
              Right (inp, ans) -> do
                DB.hset a.db (keybase pid part "inputs") bstid $ encodeUtf8 inp
                DB.hset a.db (keybase pid part "answers") bstid $ encodeUtf8 ans
                respondText inp
  onPost "/advent/puzzle/:pid/:part/submit" $ authed a \creds -> do
    pid :: Integer <- pathParam "pid"
    part <- pathParam "part"
    let bstid = encodeUtf8 creds.twitchId
    actual <- formParam "answer"
    mcheck <- fmap decodeUtf8 <$> DB.get a.db (keybase pid part "checker")
    manswer <- DB.hget a.db (keybase pid part "answers") bstid
    case (mcheck, manswer) of
      (Just check, Just expected) -> do
        FFI.checkAnswer check actual (decodeUtf8 expected) >>= \case
          Left err -> do
            status status500
            respondText $ mconcat
              [ "the checker failed to report if your answer was correct. this is probably a clonk-problem. here's the error:\n"
              , err
              ]
          Right False -> do
            respondText "that's the wrong answer, try again!"
          Right True -> do
            DB.sadd a.db (keybase pid part "solvers") [encodeUtf8 creds.twitchId]
            respondText "that's the right answer! nice work!"
      (Nothing, _) -> do
        status status500
        respondText "the puzzle is messed up and your solution could not be checked - ask clonk about it!"
      _ -> do
        status status400
        respondText "you never even looked at the input! make sure to check that out first, or there's no way you can solve the puzzle!"

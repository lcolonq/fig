module Main where

import Prelude

import Audio as Audio
import Auth (AuthInfo, authHeader, getToken, startTwitchAuth)
import Config as Config
import Data.Array (head)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Fetch (fetch)
import Model (startModel)
import UI as UI
import Web.DOM as DOM
import Web.DOM.DOMTokenList as DOM.DTL
import Web.DOM.Document (doctype)
import Web.DOM.Document as DOM.Doc
import Web.DOM.Element as DOM.El
import Web.DOM.Node as DOM.Node
import Web.DOM.NodeList as DOM.NL
import Web.DOM.NonElementParentNode as DOM.NEP
import Web.DOM.ParentNode as DOM.P
import Web.DOM.Text as DOM.Text
import Web.Event.Event as Ev
import Web.Event.EventTarget as Ev.Tar
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTML.Doc
import Web.HTML.Window as HTML.Win

maybeToArray :: forall a. Maybe a -> Array a
maybeToArray (Just x) = [x]
maybeToArray Nothing = []

byId :: forall m. MonadEffect m => String -> m DOM.Element
byId i = do
  w <- liftEffect HTML.window
  d <- liftEffect $ HTML.Doc.toDocument <$> HTML.Win.document w
  liftEffect (DOM.NEP.getElementById i (DOM.Doc.toNonElementParentNode d)) >>= case _ of
    Nothing -> liftEffect $ throw $ "could not find element with id: " <> i
    Just e -> pure e

queryAll :: forall m. MonadEffect m => String -> m (Array DOM.Element)
queryAll q = do
  w <- liftEffect HTML.window
  d <- liftEffect $ HTML.Doc.toDocument <$> HTML.Win.document w
  nl <- liftEffect (DOM.P.querySelectorAll (DOM.P.QuerySelector q) (DOM.Doc.toParentNode d))
  ns <- liftEffect $ DOM.NL.toArray nl
  pure $ fold $ (maybeToArray <<< DOM.El.fromNode) <$> ns

query :: forall m . MonadEffect m => String -> m DOM.Element
query q = do
  queryAll q >>= head >>> case _ of
    Nothing -> liftEffect $ throw $ "could not find element matching query: " <> q
    Just x -> pure x

listen :: forall m. MonadEffect m => DOM.Element -> String -> (Ev.Event -> Effect Unit) -> m Unit
listen e ev f = do
  l <- liftEffect $ Ev.Tar.eventListener f
  liftEffect $ Ev.Tar.addEventListener (Ev.EventType ev) l false $ DOM.El.toEventTarget e

create :: forall m. MonadEffect m => String -> Array String -> Array DOM.Element -> m DOM.Element
create tag classes children = do
  w <- liftEffect HTML.window
  d <- liftEffect $ HTML.Doc.toDocument <$> HTML.Win.document w
  el <- liftEffect $ DOM.Doc.createElement tag d
  cl <- liftEffect $ DOM.El.classList el
  for_ classes \c ->
    liftEffect $ DOM.DTL.add cl c
  for_ children \c ->
    appendElement el c
  pure el

appendElement :: forall m. MonadEffect m => DOM.Element -> DOM.Element -> m Unit
appendElement parent child = liftEffect $ DOM.Node.appendChild (DOM.El.toNode child) (DOM.El.toNode parent)

appendText :: forall m. MonadEffect m => DOM.Element -> String -> m Unit
appendText parent s = do
  w <- liftEffect HTML.window
  d <- liftEffect $ HTML.Doc.toDocument <$> HTML.Win.document w
  n <- liftEffect $ DOM.Doc.createTextNode s d
  liftEffect $ DOM.Node.appendChild (DOM.Text.toNode n) (DOM.El.toNode parent)

setText :: forall m. MonadEffect m => DOM.Element -> String -> m Unit
setText e s = liftEffect $ DOM.Node.setTextContent s $ DOM.El.toNode e

updateSubtitle :: Aff Unit
updateSubtitle = do
  subtitle <- byId "lcolonq-subtitle"
  { text: catchphrase } <- fetch (Config.apiServer <> "/catchphrase") {}
  catchphrase >>= setText subtitle

checkAuth :: AuthInfo -> Aff String
checkAuth auth = do
  { text: resp } <-
    fetch (Config.apiServer <> "/check")
    { headers:
      { "Authorization": authHeader auth
      }
    }
  resp

mainHomepage :: Effect Unit
mainHomepage = launchAff_ do
  liftEffect $ log "hi"
  startModel
  marq <- byId "lcolonq-marquee"
  { text: motd } <- fetch (Config.apiServer <> "/motd") {}
  motd >>= setText marq

  getToken >>= case _ of
    Just a@(Tuple t n) -> do
      liftEffect $ log t
      liftEffect $ log n
      checkAuth a >>= log >>> liftEffect
    _ -> pure unit

  updateSubtitle
  subtitle <- byId "lcolonq-subtitle"
  listen subtitle "click" \_ev -> do
    startTwitchAuth
    launchAff_ updateSubtitle
  
  for_ (Array.range 0 6) \i -> do
    letter <- byId $ "lcolonq-letter-" <> show i
    listen letter "click" \_ev -> do
      Audio.playVoice true i
    listen letter "mouseover" \_ev -> do
      Audio.playVoice false i

mainExtension :: Effect Unit
mainExtension = launchAff_ do
  liftEffect $ log "hello from extension"
  UI.setInterval 1000.0 do
    e <- query ".chat-scrollable-area__message-container"
    new <- create "div" [".chat-line__message"] []
    appendText new "test"
    appendElement e new

mainObs :: Effect Unit
mainObs = launchAff_ do
  startModel

main :: Effect Unit
main = case Config.mode of
  0 -> mainHomepage
  1 -> mainExtension
  2 -> mainObs
  _ -> throw "unknown mode"

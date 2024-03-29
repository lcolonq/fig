module Main where

import Prelude

import Config as Config
import Audio as Audio

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Fetch (fetch)
import Web.DOM as DOM
import Web.DOM.Document as DOM.Doc
import Web.DOM.Element as DOM.El
import Web.DOM.Node as DOM.Node
import Web.DOM.NonElementParentNode as DOM.NEP
import Web.Event.Event as Ev
import Web.Event.EventTarget as Ev.Tar
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTML.Doc
import Web.HTML.Window as HTML.Win

byId :: forall m. MonadEffect m => String -> m DOM.Element
byId i = do
  w <- liftEffect HTML.window
  d <- liftEffect $ HTML.Doc.toDocument <$> HTML.Win.document w
  liftEffect (DOM.NEP.getElementById i (DOM.Doc.toNonElementParentNode d)) >>= case _ of
    Nothing -> liftEffect $ throw $ "could not find element with id: " <> i
    Just e -> pure e

listen :: forall m. MonadEffect m => DOM.Element -> String -> (Ev.Event -> Effect Unit) -> m Unit
listen e ev f = do
  l <- liftEffect $ Ev.Tar.eventListener f
  liftEffect $ Ev.Tar.addEventListener (Ev.EventType ev) l false $ DOM.El.toEventTarget e

setText :: forall m. MonadEffect m => DOM.Element -> String -> m Unit
setText e s = liftEffect $ DOM.Node.setTextContent s $ DOM.El.toNode e

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "hi"
  marq <- byId "lcolonq-marquee"
  { text: motd } <- fetch (Config.apiServer <> "/motd") {}
  motd >>= setText marq

  subtitle <- byId "lcolonq-subtitle"
  { text: catchphrase } <- fetch (Config.apiServer <> "/catchphrase") {}
  catchphrase >>= setText subtitle
  
  for_ (Array.range 0 6) \i -> do
    letter <- byId $ "lcolonq-letter-" <> show i
    listen letter "click" \_ev -> do
      Audio.playVoice true i
    listen letter "mouseover" \_ev -> do
      Audio.playVoice false i

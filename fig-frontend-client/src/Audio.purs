module Audio where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import _playVoice :: Boolean -> Int -> Effect Unit
playVoice :: forall m. MonadEffect m => Boolean -> Int -> m Unit
playVoice b i = liftEffect $ _playVoice b i

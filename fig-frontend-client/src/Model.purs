module Model where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import _startModel :: Effect Unit
startModel :: forall m. MonadEffect m => m Unit
startModel = liftEffect _startModel

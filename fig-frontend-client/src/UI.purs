module UI where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import _cheatLog :: forall a. a -> Effect Unit
cheatLog :: forall m a. MonadEffect m => a -> m Unit
cheatLog x = liftEffect $ _cheatLog x

foreign import _setInterval :: Number -> Effect Unit -> Effect Unit
setInterval :: forall m. MonadEffect m => Number -> Effect Unit -> m Unit
setInterval d f = liftEffect $ _setInterval d f

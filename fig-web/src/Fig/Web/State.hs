{-# Language TemplateHaskell #-}

module Fig.Web.State where

import Control.Lens.TH (makeLensesFor)
import Control.Lens ((<>=))
import Control.Monad.State (runStateT)

import Fig.Prelude

import qualified Data.IORef as IORef

newtype State = State
  { buffer :: Text
  }
makeLensesFor [("buffer", "buffer")] ''State

defaultState :: State
defaultState = State
  { buffer = ""
  }

type StateRef = IORef.IORef State

stateRef :: IO StateRef
stateRef = IORef.newIORef defaultState

withState ::
  MonadIO m' =>
  StateRef ->
  (forall m. (MonadIO m, MonadState State m) => m a) ->
  m' a
withState ref f = do
  s <- liftIO $ IORef.readIORef ref
  (res, s') <- liftIO $ runStateT f s
  liftIO $ IORef.writeIORef ref s'
  pure res

sayHi :: StateRef -> IO ()
sayHi ref = withState ref do
  buffer <>= "hi"

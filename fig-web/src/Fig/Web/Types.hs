module Fig.Web.Types
  ( LiveEvent(..)
  , Commands(..)
  , Channels(..)
  , newChannels
  , Globals(..)
  , newGlobals
  , DB(..)
  , ModuleArgs(..)
  , Module
  , Websockets
  , BusEvents
  ) where

import Fig.Prelude

import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import qualified Data.Set as Set

import qualified Network.WebSockets as WS

import qualified Web.Scotty as Sc

import qualified Database.Redis as Redis

import Fig.Bus.Binary.Client
import Fig.Web.Utils

data LiveEvent
  = LiveEventOnline !(Set.Set Text)
  | LiveEventOffline !(Set.Set Text)
  deriving (Show, Eq, Ord)

data Channels = Channels
  { live :: !(Chan.Chan LiveEvent)
  , gizmo :: !(Chan.Chan Text)
  , model :: !(Chan.Chan WS.DataMessage)
  }

newChannels :: IO Channels
newChannels = do
  live <- Chan.newChan
  gizmo <- Chan.newChan
  model <- Chan.newChan
  pure Channels {..}

newtype Globals = Globals
  { currentlyLive :: MVar.MVar (Set.Set Text)
  }

newGlobals :: IO Globals
newGlobals = do
  currentlyLive <- MVar.newMVar Set.empty
  pure Globals {..}

newtype DB = DB { conn :: Redis.Connection }

data ModuleArgs = ModuleArgs
  { cfg :: Config
  , cmds :: Commands IO
  , db :: DB
  , globals :: Globals
  , channels :: Channels
  }

type Module = ModuleArgs -> Sc.ScottyM ()
type Websockets = ModuleArgs -> [WebsocketHandler]
type BusEvents = ModuleArgs -> [BusEventHandler]

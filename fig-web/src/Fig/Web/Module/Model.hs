module Fig.Web.Module.Model
  ( publicWebsockets
  ) where

import Fig.Prelude

import qualified Control.Concurrent.Chan as Chan

import qualified Network.WebSockets as WS

import Fig.Web.Types

publicWebsockets :: Websockets
publicWebsockets a =
  [ ( "/api/model/broadcast", \conn -> do
        forever do
          msg <- liftIO $ WS.receiveDataMessage conn
          Chan.writeChan a.channels.model msg
    )
  , ( "/api/model/events", \conn -> do
        c <- Chan.dupChan a.channels.model
        forever do
          ev <- liftIO $ Chan.readChan c
          WS.sendDataMessage conn ev
    )
  ]

module Fig.Web.Module.ShindigsSorting
  ( public
  , publicWebsockets
  ) where

import Fig.Prelude

import qualified Control.Concurrent.Chan as Chan

import qualified Data.Aeson as Aeson

import qualified Network.WebSockets as WS

import Fig.Web.Utils
import Fig.Web.Types

public :: PublicModule
public a = do
  onPost "/api/shindigssort" do
    b :: ShindigsSort <- bodyJSON
    liftIO $ Chan.writeChan a.channels.shindigssort b

publicWebsockets :: PublicWebsockets
publicWebsockets a =
  [ ( "/api/shindigssort/events", \conn -> do
        c <- Chan.dupChan a.channels.shindigssort
        forever do
          ev <- liftIO $ Chan.readChan c
          WS.sendTextData conn $ Aeson.encode ev
    )
  ]

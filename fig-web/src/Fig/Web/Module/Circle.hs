module Fig.Web.Module.Circle
  ( public
  , publicWebsockets
  , publicBusEvents
  ) where

import Fig.Prelude

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Chan as Chan

import qualified Data.Text as Text
import qualified Data.Set as Set

import qualified Network.WebSockets as WS

import Fig.Utils.SExpr
import Fig.Web.Utils
import Fig.Web.Types

public :: PublicModule
public a = do
  onGet "/api/circle" do
    live <- liftIO $ MVar.readMVar a.globals.currentlyLive
    respondText $ pretty . SExprList @Void $ SExprString <$> Set.toList live

publicWebsockets :: PublicWebsockets
publicWebsockets a =
  [ ( "/api/circle/events", \conn -> do
        c <- Chan.dupChan a.channels.live
        forever do
          ev <- liftIO $ Chan.readChan c
          WS.sendTextData conn $ case ev of
            LiveEventOnline online ->
              pretty $ SExprList @Void
              [ SExprString "online"
              , SExprList $ SExprString <$> Set.toList online
              ]
            LiveEventOffline offline ->
              pretty $ SExprList @Void
              [ SExprString "offline"
              , SExprList $ SExprString <$> Set.toList offline
              ]
    )
  ]

publicBusEvents :: PublicBusEvents
publicBusEvents a =
  [ ("monitor twitch stream online", \d -> do
        let dstr = decodeUtf8 d
        let live = Text.words dstr
        let new = Set.fromList live 
        old <- MVar.swapMVar a.globals.currentlyLive new
        let online = Set.difference new old
        let offline = Set.difference old new
        unless (Set.null online && Set.null offline) do
          log $ "Newly online: " <> Text.intercalate " " (Set.toList online) <> ", newly offline: " <> Text.intercalate " " (Set.toList offline)
        unless (Set.null online) . Chan.writeChan a.channels.live $ LiveEventOnline online
        unless (Set.null offline) . Chan.writeChan a.channels.live $ LiveEventOnline offline
    )
  ]

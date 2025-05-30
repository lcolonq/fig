module Fig.Web.Module.Gizmo
  ( public
  , publicWebsockets
  , publicBusEvents
  ) where

import Fig.Prelude

import qualified Control.Concurrent.Chan as Chan

import qualified Data.Text as Text

import qualified Network.WebSockets as WS

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/gizmo" do
    buf <- queryParam "buf"
    DB.hget a.db "gizmos" buf >>= \case
      Nothing -> do
        status status404
        respondText "gizmo does not exist"
      Just html -> respondHTML $ decodeUtf8 html
  onGet "/api/gizmo/list" do
    gizmos <- maybe [] (fmap decodeUtf8) <$> DB.hkeys a.db "gizmos"
    respondText $ Text.unlines gizmos

publicWebsockets :: PublicWebsockets
publicWebsockets a =
  [ ( "/api/gizmo/events", \conn -> do
        c <- Chan.dupChan a.channels.gizmo
        forever do
          ev <- liftIO $ Chan.readChan c
          WS.sendTextData conn ev
    )
  ]

publicBusEvents :: PublicBusEvents
publicBusEvents a =
  [ ("gizmo buffer update", \d -> do
        let dstr = decodeUtf8 d
        let updates = Text.splitOn " " dstr
        forM_ updates $ Chan.writeChan a.channels.gizmo
    )
  ]

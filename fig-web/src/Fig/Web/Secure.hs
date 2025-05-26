module Fig.Web.Secure
  ( server
  ) where

import Fig.Prelude

import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp

import qualified Web.Scotty as Sc

import Fig.Bus.Binary.Client
import Fig.Web.Types
import Fig.Web.Utils
import qualified Fig.Web.DB as DB
import qualified Fig.Web.Module.Exchange as Exchange
import qualified Fig.Web.Module.Exchange as Redeem

allBusEvents :: ModuleArgs -> BusEventHandlers
allBusEvents args = busEvents . mconcat $ fmap ($ args)
  [ 
  ]

server :: Config -> (Text, Text) -> IO ()
server cfg busAddr = do
  log $ "Web server running on port " <> tshow cfg.port
  log "Connecting to database..."
  db <- DB.connect cfg
  channels <- newChannels
  globals <- newGlobals
  busClient busAddr
    (\cmds -> do
        log "Connected to bus!"
        let args = ModuleArgs{..}
        subscribeBusEvents cmds $ allBusEvents args
        Warp.run cfg.port =<< app args
    )
    (\cmds ev d -> do
        let args = ModuleArgs{..}
        handleBusEvent (allBusEvents args) ev d
    )
    (pure ())

app :: ModuleArgs -> IO Wai.Application
app args = do
  log "Connected! Secure server active."
  Sc.scottyApp do
    Sc.middleware . Wai.Static.staticPolicy $ mconcat
      [ Wai.Static.isNotAbsolute
      , Wai.Static.only
        [ ("menu", "menu.html")
        , ("throwshade", "throwshade.html")
        , ("main.css", "main.css")
        , ("main.js", "main.js")
        ]
        Wai.Static.<|> Wai.Static.hasPrefix "assets"
        Wai.Static.<|> Wai.Static.hasPrefix "newton"
      , Wai.Static.addBase args.cfg.assetPath
      ]
    onGet "/" do
      respondText "this is the secure endpoint"
    onGet "/api/status" do
      respondText "this is the secure endpoint"
    onGet "/api/info" $ authed \creds -> do
      respondText $ creds.user <> " " <> creds.email
    Exchange.secure args
    Redeem.secure args
    Sc.notFound do
      respondText "not found"

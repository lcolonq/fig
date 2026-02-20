module Fig.Web.Public
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
import qualified Fig.Web.Module.Misc as Misc
import qualified Fig.Web.Module.TwitchAuth as TwitchAuth
import qualified Fig.Web.Module.Exchange as Exchange
import qualified Fig.Web.Module.Gizmo as Gizmo
import qualified Fig.Web.Module.Sentiment as Sentiment
import qualified Fig.Web.Module.Circle as Circle
import qualified Fig.Web.Module.Model as Model
import qualified Fig.Web.Module.Bells as Bells
import qualified Fig.Web.Module.User as User
import qualified Fig.Web.Module.Shader as Shader
import qualified Fig.Web.Module.HLS as HLS
import qualified Fig.Web.Module.TCG as TCG
import qualified Fig.Web.Module.Debt as Debt
import qualified Fig.Web.Module.ShindigsSorting as ShindigsSorting

allBusEvents :: PublicModuleArgs -> BusEventHandlers
allBusEvents args = busEvents . mconcat $ fmap ($ args)
  [ Gizmo.publicBusEvents
  , Circle.publicBusEvents
  ]

server :: PublicOptions -> Config -> (Text, Text) -> IO ()
server options cfg busAddr = do
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

app :: PublicModuleArgs -> IO Wai.Application
app args = do
  log "Connected! Server active."
  Sc.scottyApp do
    Sc.middleware . Wai.Static.staticPolicy $ mconcat
      [ Wai.Static.isNotAbsolute
      , Wai.Static.only
        [ ("register", "register.html")
        , ("gizmo", "gizmo.html")
        , ("advent", "advent.html")
        , ("debt", "debtclock.html")
        , ("charsheet", "charsheet-public.html")
        , ("main.js", "main.js")
        ] Wai.Static.<|> Wai.Static.hasPrefix "assets"
      , Wai.Static.addBase args.cfg.assetPath
      ]
    onGet "/unauthorized" do
      status status401
      respondText $ mconcat
        [ "your request was rejected because that endpoint requires authentication\n"
        , "you can log in by POSTing your credentials to https://auth.colonq.computer/api/firstfactor\n"
        , "for example:\n"
        , "  curl https://auth.colonq.computer/api/firstfactor \\\n"
        , "    --header \"Content-Type: application/json\" \\\n"
        , "    --request POST \\\n"
        , "    --data '{\"username\":\"AzureDiamond\",\"password\":\"hunter2\"}' \\\n"
        , "    --cookie-jar cookies.txt\n"
        , "this will write a cookie called \"authelia_session\" to cookies.txt\n"
        , "send this cookie along with your requests to use the secure endpoints\n"
        , "for example:\n"
        , "  curl https://secure.colonq.computer --cookie cookies.txt\n"
        ]
    Misc.public args
    TwitchAuth.public args
    Exchange.public args
    Gizmo.public args
    Sentiment.public args
    Circle.public args
    Bells.public args
    User.public args
    Shader.public args
    HLS.public args
    TCG.public args
    Debt.public args
    ShindigsSorting.public args
    websocket $ mconcat
      [ Gizmo.publicWebsockets args
      , Circle.publicWebsockets args
      , Model.publicWebsockets args
      , ShindigsSorting.publicWebsockets args
      ]
    Sc.notFound do
      respondText "not found"

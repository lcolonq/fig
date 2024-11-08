{-# Language QuasiQuotes #-}

module Fig.Web.Secure where

import Fig.Prelude

import System.Random (randomRIO)

import Control.Monad (unless)
import Control.Lens (use, (^?), Ixed (..))
import qualified Control.Concurrent.Chan as Chan
import qualified Control.Concurrent.MVar as MVar

import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.L
import qualified Data.ByteString.Base64 as BS.Base64
import qualified Data.Set as Set

import qualified Network.Wai as Wai
-- import qualified Network.Wai.Middleware.Static as Wai.Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WS

import qualified Web.Scotty as Sc

import Fig.Utils.SExpr
import Fig.Bus.Client
import Fig.Web.Utils
import Fig.Web.Auth
import Fig.Web.State
import qualified Fig.Web.DB as DB

data LiveEvent
  = LiveEventOnline !(Set.Set Text)
  | LiveEventOffline !(Set.Set Text)
  deriving (Show, Eq, Ord)

server :: Config -> (Text, Text) -> IO ()
server cfg busAddr = do
  log $ "Web server running on port " <> tshow cfg.port
  busClient busAddr
    (\cmds -> do
        log "Connected to bus!"
        Warp.run cfg.port =<< app cfg cmds
    )
    (\_cmds d -> do
        log $ "Invalid event: " <> tshow d
    )
    (pure ())

sexprStr :: Text -> SExpr
sexprStr = SExprString . BS.Base64.encodeBase64 . encodeUtf8

app :: Config -> Commands IO -> IO Wai.Application
app cfg cmds = do
  log "Connecting to database..."
  db <- DB.connect cfg
  log "Connected! Secure server active."
  Sc.scottyApp do
    Sc.get "/" do
      Sc.text "this is the secure endpoint"
    Sc.notFound do
      Sc.text "not found"

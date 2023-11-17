{-# Language QuasiQuotes #-}
{-# Language RecordWildCards #-}
{-# Language ApplicativeDo #-}

module Fig.Monitor.Bullfrog
  ( bullfrogClient
  ) where

import Fig.Prelude

import qualified Data.Text as Text

import qualified Wuss as WS
import qualified Network.WebSockets.Connection as WS

import Fig.Utils.SExpr
import Fig.Bus.Client
import Fig.Monitor.Bullfrog.Utils

bullfrogClient :: Config -> (Text, Text) -> IO ()
bullfrogClient cfg busAddr = do
  WS.runSecureClient "colonq.computer" 443 ("/bullfrog/api/channel/broadcast?token=" <> Text.unpack cfg.authToken) \conn -> do
    busClient busAddr
      (\cmds -> do
          log "Connected to bus and broadcast server"
          cmds.subscribe [sexp|(monitor bullfrog broadcast)|]
      )
      (\_cmds d -> do
          case d of
            SExprList [ev, SExprString msg]
              | ev == [sexp|(monitor bullfrog broadcast)|] -> do
                  log $ "Broadcasting message: " <> msg
                  WS.sendTextData conn msg
            _ -> log $ "Invalid incoming message: " <> tshow d
      )
      (pure ())

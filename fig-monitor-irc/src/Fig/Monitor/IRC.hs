{-# Language QuasiQuotes #-}

module Fig.Monitor.IRC where

import Fig.Prelude

import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as BS.Base64

import Lens.Micro ((%~), (.~), (^.))

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Chan as Chan

import qualified Network.IRC.Client as IRC

import Fig.Utils.SExpr
import Fig.Bus.Client
import Fig.Monitor.IRC.Utils

data OutgoingMessage = OutgoingMessage
  { user :: Text
  , msg :: Text
  }

srcUser :: IRC.Source a -> Maybe a
srcUser (IRC.Channel _ user) = Just user
srcUser (IRC.User user) = Just user
srcUser _ = Nothing

ircBot :: Config -> (Text, Text) -> IO ()
ircBot cfg busAddr = do
  outgoing <- Chan.newChan @OutgoingMessage
  mircst <- Conc.newEmptyMVar
  void . Conc.forkIO $ Conc.readMVar mircst >>= \ircst -> forever $ do
    o <- liftIO $ Chan.readChan outgoing
    log $ "Sending: " <> o.msg <> " (from " <> o.user <> ")"
    let msg = IRC.Privmsg cfg.sendchannel . Right . Text.take 400 $ mconcat
          [ "<", o.user, "> "
          , Text.replace "\n" " " o.msg
          ]
    IRC.runIRCAction (IRC.send msg) ircst
  busClient busAddr
    (\cmds -> do
        cmds.subscribe [sexp|(monitor irc chat outgoing)|]
        let handler = IRC.EventHandler
              ( \case
                  ev
                    | IRC.Privmsg _ (Right msg) <- ev ^. IRC.message -> Just msg
                    | otherwise -> Nothing 
              )
              ( \src msg -> case srcUser src of
                  Just user -> do
                    log $ "Received: " <> msg <> " (from " <> user <> ")"
                    liftIO $ cmds.publish [sexp|(monitor irc chat incoming)|]
                      [ SExprString . BS.Base64.encodeBase64 . encodeUtf8 $ user
                      , SExprList []
                      , SExprString . BS.Base64.encodeBase64 . encodeUtf8 $ msg
                      ]
                  Nothing -> pure ()
              )
        ircst <- IRC.newIRCState
          ( IRC.tlsConnection (IRC.WithDefaultConfig (encodeUtf8 cfg.host) cfg.port)
          -- ( IRC.plainConnection (encodeUtf8 cfg.host) cfg.port
          )
          ( IRC.defaultInstanceConfig cfg.nick
            & IRC.handlers %~ (handler:)
            & IRC.channels .~ cfg.channels 
          )
          ()
        Conc.putMVar mircst ircst
        IRC.runClientWith ircst
    )
    (\_cmds d -> do
        case d of
          SExprList [ev, SExprString euser, SExprString emsg]
            | ev == [sexp|(monitor irc chat outgoing)|]
            , Right user <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 euser)
            , Right msg <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 emsg) -> do
                Chan.writeChan outgoing OutgoingMessage { user = user, msg = msg }
          _ -> log $ "Invalid outgoing message: " <> tshow d
    )
    (pure ())

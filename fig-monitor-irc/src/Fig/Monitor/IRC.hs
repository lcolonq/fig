{-# Language QuasiQuotes #-}

module Fig.Monitor.IRC where

import Fig.Prelude

import Control.Monad (unless)

import Data.Functor ((<&>))
import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as BS.Base64

import Lens.Micro ((%~), (.~), (^.))

import qualified Control.Concurrent as Conc
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan

import qualified Network.IRC.Client as IRC

import Fig.Utils.SExpr
import Fig.Bus.SExpr.Client
import Fig.Monitor.IRC.Utils

data OutgoingMessage = OutgoingMessage
  { chan :: !Text
  , user :: !Text
  , msg :: !Text
  }

splitLineIRCLength :: Text -> [Text]
splitLineIRCLength x
  | Text.length x > 300 =
    let (m, xs) = Text.splitAt 300 x
    in m : splitLineIRCLength xs
  | otherwise = [x]

ircBot :: Config -> (Text, Text) -> IO ()
ircBot cfg busAddr = do
  outgoing <- Chan.newChan @OutgoingMessage
  mircst <- Conc.newEmptyMVar
  Async.concurrently_
    ( Conc.readMVar mircst >>= \ircst -> forever $ do
        o <- liftIO $ Chan.readChan outgoing
        log $ "Sending: " <> o.msg <> " (from " <> o.user <> ")"
        let lines = Text.splitOn "\n" o.msg
        let linesshort = mconcat $ splitLineIRCLength <$> lines
        let msgs = linesshort <&> \msg -> IRC.Privmsg o.chan . Right . Text.take 400 $ mconcat
              [ "<", o.user, "> "
              , Text.replace "\n" " " msg
              ]
        IRC.runIRCAction (forM_ msgs IRC.send) ircst
    )
    do
      busClient busAddr
        (\cmds -> do
            cmds.subscribe [sexp|(monitor irc chat outgoing)|]
            let handler = IRC.EventHandler
                  ( \case
                      ev
                        | IRC.Privmsg _ (Right msg) <- ev ^. IRC.message -> Just msg
                        | otherwise -> Nothing 
                  )
                  ( \src msg -> case src of
                      IRC.Channel chan user -> do
                        log $ "Received: " <> msg <> " (from " <> user <> ")"
                        liftIO $ cmds.publish [sexp|(monitor irc chat incoming)|]
                          [ SExprString chan
                          , SExprString . BS.Base64.encodeBase64 . encodeUtf8 $ user
                          , SExprList []
                          , SExprString . BS.Base64.encodeBase64 . encodeUtf8 $ msg
                          ]
                      _ -> pure ()
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
            log "Connecting to IRC server..."
            IRC.runClientWith ircst
        )
        (\_cmds d -> do
            case d of
              SExprList [ev, SExprString chan, SExprString euser, SExprString emsg]
                | ev == [sexp|(monitor irc chat outgoing)|]
                , Right user <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 euser)
                , Right msg <- decodeUtf8 <$> BS.Base64.decodeBase64 (encodeUtf8 emsg) -> do
                    unless (any (Text.isInfixOf user) (["fabius"] :: [Text])) do
                      Chan.writeChan outgoing OutgoingMessage { chan, user, msg = msg }
              _ -> log $ "Invalid outgoing message: " <> tshow d
        )
        (pure ())

{-# Language QuasiQuotes #-}

module Fig.Bridge.IRCDiscord where

import Fig.Prelude

import qualified Data.List as List

import Fig.Bridge.IRCDiscord.Utils
import Fig.Utils.SExpr
import Fig.Bus.SExpr.Client

bridge :: Config -> (Text, Text) -> IO ()
bridge cfg busAddr = do
  busClient busAddr
    (\cmds -> do
        cmds.subscribe [sexp|(monitor irc chat incoming)|]
        cmds.subscribe [sexp|(monitor discord chat incoming)|]
    )
    (\cmds d -> do
        case d of
          SExprList [ev, tchan, user, _, msg]
            | ev == [sexp|(monitor irc chat incoming)|]
            , SExprString chan <- tchan ->
              case List.find ((== chan) . snd) cfg.mapping of
                Nothing -> log $ mconcat
                  [ "Message on unmapped IRC channel: " <> tshow chan
                  ]
                Just (dchan, _) -> do
                  log $ mconcat
                    [ "Incoming message on IRC channel " <> tshow chan
                    , ", bridging to Discord channel " <> tshow dchan
                    ]
                  cmds.publish [sexp|(monitor discord chat outgoing)|]
                    [ SExprInteger $ fromIntegral dchan
                    , user
                    , msg
                    ]
            | ev == [sexp|(monitor discord chat incoming)|]
            , SExprInteger chan <- tchan ->
              case List.find ((== fromInteger chan) . fst) cfg.mapping of
                Nothing -> log $ mconcat
                  [ "Message on unmapped Discord channel: " <> tshow chan
                  ]
                Just (_, ichan) -> do
                  log $ mconcat
                    [ "Incoming message on Discord channel " <> tshow chan
                    , ", bridging to IRC channel " <> ichan
                    ]
                  cmds.publish [sexp|(monitor irc chat outgoing)|]
                    [ SExprString ichan
                    , user
                    , msg
                    ]
          _ -> log $ "Invalid message: " <> tshow d
    )
    (pure ())

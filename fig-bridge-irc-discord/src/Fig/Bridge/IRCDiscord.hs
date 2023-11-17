{-# Language QuasiQuotes #-}

module Fig.Bridge.IRCDiscord where

import Fig.Prelude

import Fig.Utils.SExpr
import Fig.Bus.Client

bridge :: (Text, Text) -> IO ()
bridge busAddr = do
  busClient busAddr
    (\cmds -> do
        cmds.subscribe [sexp|(monitor irc chat incoming)|]
        cmds.subscribe [sexp|(monitor discord chat incoming)|]
    )
    (\cmds d -> do
        case d of
          SExprList [ev, user, _, msg]
            | ev == [sexp|(monitor irc chat incoming)|] ->
              cmds.publish [sexp|(monitor discord chat outgoing)|]
                [ user
                , msg
                ]
            | ev == [sexp|(monitor discord chat incoming)|] ->
              cmds.publish [sexp|(monitor irc chat outgoing)|]
                [ user
                , msg
                ]
          _ -> log $ "Invalid message: " <> tshow d
    )
    (pure ())

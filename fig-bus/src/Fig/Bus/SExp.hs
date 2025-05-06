module Fig.Bus.SExp (main) where

import Fig.Prelude

import Control.Concurrent.MVar as MVar

import qualified Data.List as List
import Data.ByteString (hPut, hGetLine)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IORef as IORef

import Fig.Utils.SExpr
import Fig.Utils.Net

newtype BusState = BusState
  { subscriptions :: Map SExpr [Handle]
  }

subscribe :: SExpr -> Handle -> BusState -> BusState
subscribe ev h bs = bs
  { subscriptions = Map.insertWith (<>) ev [h] bs.subscriptions
  }

unsubscribe :: SExpr -> Handle -> BusState -> BusState
unsubscribe ev h bs = bs
  { subscriptions = Map.update (Just . List.delete h) ev bs.subscriptions
  }

publish :: SExpr -> [SExpr] -> BusState -> IO ()
publish ev d bs =
  case Map.lookup ev bs.subscriptions of
    Nothing -> pure ()
    Just hs -> forM_ hs \h -> do
      hPut h . encodeUtf8 $ pretty (SExprList $ ev:d) <> "\n"

main :: (Maybe Text, Text) -> IO ()
main bind = do
  st <- MVar.newMVar $ BusState { subscriptions = Map.empty }
  server bind do
    subs <- IORef.newIORef ([] :: [SExpr])
    pure \h peer ->
      ( do
          forever do
            line <- throwLeft id . decodeUtf8' =<< hGetLine h
            case parseSExpr line of
              Just (SExprList (SExprSymbol "ping":_)) -> do
                log $ tshow peer <> " pinged"
                hPut h . encodeUtf8 $ "(pong)\n"
              Just (SExprList [SExprSymbol "sub", ev]) -> do
                log $ tshow peer <> " subscribing to: " <> pretty ev
                IORef.modifyIORef' subs (ev:)
                MVar.modifyMVar_ st (pure . subscribe ev h)
              Just (SExprList (SExprSymbol "pub":ev:d)) -> do
                log $ tshow peer <> " publishing " <> pretty (SExprList d) <> " to: " <> pretty ev
                publish ev d =<< MVar.readMVar st
              Just x -> log $ tshow peer <> " sent invalid command: " <> pretty x
              Nothing -> log $ tshow peer <> " sent malformed s-expression: " <> line
      , do
          ss <- IORef.readIORef subs
          MVar.modifyMVar_ st \bs -> pure $ foldr (`unsubscribe` h) bs ss
      )

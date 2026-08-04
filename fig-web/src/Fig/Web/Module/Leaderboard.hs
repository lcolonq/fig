module Fig.Web.Module.Leaderboard
  ( public
  ) where

import Fig.Prelude

import qualified Lucid as L
import Web.Scotty (ScottyException)

import Fig.Web.Utils
import Fig.Web.Types
import Fig.Utils.DB (DB)
import qualified Fig.Utils.DB as DB

leaderboardKey :: ByteString -> ByteString
leaderboardKey = ("leaderboard:"<>)

fetchLeaderboard :: MonadIO m => DB -> ByteString -> Int -> m [(Text, Text, Double)]
fetchLeaderboard db lid count = DB.run db do
  top <- DB.zrevrange (leaderboardKey lid) 0 $ fromIntegral count
  forM top $ \(uid, score) -> do
    name <- fromMaybe uid <$> DB.hget ("user:properties:" <> uid) "name"
    pure (decodeUtf8 uid, decodeUtf8 name, score)

public :: PublicModule
public a = do
  onGet "/leaderboard/:lid" do
    lid <- pathParam "lid"
    top10 <- fetchLeaderboard a.db lid 10
    respondHTML do
      head_ . title_ . L.toHtml $ "LCOLONQ Leaderboard: " <> decodeUtf8 lid
      body_ do
        ol_ $ forM_ top10 $ \(_, key, score) -> do
          li_ . L.toHtml $ key <> ": " <> tshow score
  onGet "/api/leaderboard/:lid" do
    lid <- pathParam "lid"
    count :: Maybe Int <- catch (Just <$> queryParam "n") \(_ :: ScottyException) -> pure Nothing
    case count of
      Just c | c > 0 -> respondJSON =<< fetchLeaderboard a.db lid (c - 1)
      _ -> respondJSON @[()] []

module Fig.Monitor.Twitch.LiveChecker
  ( twitchChannelLiveChecker
  ) where

import Fig.Prelude

import Control.Concurrent (threadDelay)

import qualified Data.Text as Text
import qualified Data.Set as Set

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Fig.Bus.Binary.Client
import Fig.Monitor.Twitch.Auth.AppToken
import Fig.Monitor.Twitch.Utils

twitchChannelLiveChecker :: Config -> (Text, Text) -> IO ()
twitchChannelLiveChecker cfg busAddr = do
  busClient busAddr
    (\cmds -> do
        let
          loop :: IO ()
          loop = do
            log "Updating liveness..."
            live <- runAuthed cfg $ usersAreLive cfg.monitor
            if null live
              then log "Update complete! No users live"
              else log $ "Update complete! Live users: " <> Text.unwords (Set.toList live)
            cmds.publish "fig monitor twitch stream online" . encodeUtf8 . Text.unwords $ Set.toList live
            threadDelay $ 5 * 60 * 1000000 -- wait 5 minutes
            loop
        loop
    )
    (\_cmds _ev _d -> pure ())
    (pure ())

usersAreLive :: [Text] -> Authed (Set.Set Text)
usersAreLive users = do
  log $ "Checking liveness for: " <> Text.intercalate " " users
  res <- authedRequestJSON @()
    "GET"
    ( mconcat
      [ "https://api.twitch.tv/helix/streams?type=live"
      , mconcat $ ("&user_login="<>) <$> users
      ]
    )
    Nothing
  let mos = flip Aeson.parseEither res \obj -> do
        obj .: "data" >>= \case
          Aeson.Array os -> catMaybes . toList <$> forM os \case
            Aeson.Object o -> Just <$> o .: "user_login"
            _else -> pure Nothing
          _else -> mempty
  case mos of
    Left err -> throwM $ FigMonitorTwitchException $ "Failed to check liveness: " <> pack err <> "\nResponse was: " <> tshow res
    Right os -> pure . Set.fromList $ filter (`elem` os) users

{-# Language ApplicativeDo #-}

module Fig.Monitor.Twitch
  ( twitchChatbot
  , twitchEventMonitor
  , twitchChannelLiveChecker
  , userTokenRedirectServer
  ) where

import Fig.Prelude

import qualified Data.Text.Lazy as Text.Lazy

import Data.Default.Class (def)

import Network.Wai.Handler.Warp (setPort)
import qualified Web.Scotty as Scotty

import Fig.Monitor.Twitch.Utils
import Fig.Monitor.Twitch.LiveChecker
import Fig.Monitor.Twitch.EventMonitor
import Fig.Monitor.Twitch.Chatbot

userTokenRedirectServer :: Config -> Bool -> IO ()
userTokenRedirectServer cfg rw = do
  log "Starting token redirect server on port 4444"
  Scotty.scottyOpts opts do
    Scotty.get "/" do
      Scotty.html $ mconcat
        [ "<a href=\"https://id.twitch.tv/oauth2/authorize?response_type=token"
        , "&client_id=", Text.Lazy.fromStrict cfg.clientId
        , "&redirect_uri=http://localhost:4444"
        , "&scope=", Text.Lazy.replace ":" "%3A" $ Text.Lazy.intercalate "+" scopes
        , "\">Authenticate</a>"
        ]
  where
    opts = Scotty.Options
      { Scotty.verbose = 0
      , Scotty.settings = setPort 4444 (Scotty.settings def)
      }
    scopes = if rw then scopesReadWrite else scopesReadOnly
    scopesReadWrite =
      [ "channel:manage:polls"
      , "channel:manage:predictions"
      , "channel:manage:redemptions"
      , "channel:manage:vips"
      , "channel:read:polls"
      , "channel:read:predictions"
      , "channel:read:redemptions"
      , "channel:read:subscriptions"
      , "channel:read:vips"
      , "channel:moderate"
      , "moderator:read:followers"
      , "moderator:read:chatters"
      , "moderator:manage:shoutouts"
      , "chat:edit"
      , "chat:read"
      , "bits:read"
      ]
    scopesReadOnly =
      [
      ]

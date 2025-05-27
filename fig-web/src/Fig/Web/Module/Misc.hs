module Fig.Web.Module.Misc
  ( public
  ) where

import Fig.Prelude

import System.Random (randomRIO)

import Control.Lens ((^?), Ixed (..))

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: Module
public a = do
  onGet "/api/motd" do
    log "getting motd"
    DB.get a.db "motd" >>= \case
      Nothing -> respondText ""
      Just val -> respondText $ decodeUtf8 val
  onGet "/api/catchphrase" do
    let catchphrases =
          [ "vtuber (male)"
          , "man of letters"
          , "cool guy, online"
          , "internet clown man"
          , "professional emacs fan"
          , "web freak"
          , "guy who really likes programming"
          , "i use nixos btw"
          , "(are these funny or cringe or both?)"
          , "haha yay"
          , "Joel"
          ] :: [Text]
    i <- randomRIO (0, length catchphrases - 1)
    case catchphrases ^? ix i of
      Nothing -> respondText "man of letters"
      Just val -> respondText val

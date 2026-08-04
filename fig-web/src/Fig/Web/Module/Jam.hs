module Fig.Web.Module.Jam
  ( public
  ) where

import Fig.Prelude

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Base16.Types as Base16
import qualified Data.ByteString.Base16 as Base16

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Utils.DB as DB

public :: PublicModule
public a = do
  onPost "/api/jam/2026/score" do
    score :: Integer <- formParam "score"
    ident <- Base16.extractBase16 . Base16.encodeBase16 . SHA256.hash <$> requestAddress
    DB.run a.db $ DB.zadd "leaderboard:jam2026" [(fromIntegral score, encodeUtf8 ident)]

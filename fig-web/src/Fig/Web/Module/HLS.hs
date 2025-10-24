module Fig.Web.Module.HLS
  ( public
  ) where

import Fig.Prelude

import Data.Functor ((<&>))

import Fig.Web.Utils
import Fig.Web.Types
import qualified Fig.Web.DB as DB

public :: PublicModule
public a = do
  onGet "/api/hls.m3u8" do
    mseq :: Maybe Integer <- ((readMaybe . unpack . decodeUtf8)=<<) <$> DB.get a.db "hlssequence"
    mlen <- DB.llen a.db "hlssamples"
    case (mseq, mlen) of
      (Just seq, Just len) -> do
        let startingSeq = seq - (len - 1)
        respondText $ mconcat
          [ "#EXTM3U\n"
          , "#EXT-X-VERSION: 4\n"
          , "#EXT-X-TARGETDURATION:5\n"
          , "#EXT-X-MEDIA-SEQUENCE:", tshow startingSeq, "\n"
          , mconcat $ reverse [0,1..len] <&> \idx -> mconcat
            [ "#EXTINF:3.0,\n"
            , "http://localhost:8080/api/hls/", tshow idx, "/sample.aac\n"
            ]
          ]
      _ -> do
        status status404
        respondText "no HLS stream"
    pure ()
  onGet "/api/hls/:num/sample.aac" do
    num <- pathParam "num"
    DB.lindex a.db "hlssamples" num >>= \case
      Nothing -> do
        status status404
        respondText "sample not found"
      Just val -> respondBytes val

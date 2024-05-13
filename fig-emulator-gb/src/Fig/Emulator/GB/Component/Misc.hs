module Fig.Emulator.GB.Component.Misc where

import Fig.Prelude

import Fig.Emulator.GB.Utils
import Fig.Emulator.GB.Bus

newtype MiscError = MiscError Text
  deriving Show
instance Exception MiscError
instance Pretty MiscError where
  pretty (MiscError b) = mconcat
    [ "misc component error: "
    , b
    ]

compMisc :: Component
compMisc = Component
  { compState = ()
  , compMatches = (== 0xff4d)
  , compUpdate = \s _ -> pure s
  , compWrite = \s _ _ -> pure s
  , compRead = \_ _ -> pure 0x00
  }

module Fig.Emulator.GB.Component.Joystick where

import Fig.Prelude

import Fig.Emulator.GB.Bus

newtype JoystickError = JoystickError Text
  deriving Show
instance Exception JoystickError
instance Pretty JoystickError where
  pretty (JoystickError b) = mconcat
    [ "joystick error: "
    , b
    ]

compJoystick :: Component
compJoystick = Component
  { compState = ()
  , compMatches = (== 0xff00)
  , compUpdate = \s _ -> pure s
  , compWrite = \s _ _ -> pure s
  , compRead = \_ _ -> pure 0x00
  }

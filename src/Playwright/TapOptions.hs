module Playwright.TapOptions
  ( TapOptions (..)
  , defaultTapOptions
  ) where

import Playwright.KeyboardModifier
  ( KeyboardModifier
  )
import Playwright.Point
  ( Point
  )
import Playwright.Timeout
  ( Timeout
  )

data TapOptions = TapOptions
  { tapOptionsForce :: Maybe Bool
  , tapOptionsModifiers :: Maybe [KeyboardModifier]
  , tapOptionsPosition :: Maybe Point
  , tapOptionsTimeout :: Maybe Timeout
  , tapOptionsTrial :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultTapOptions :: TapOptions
defaultTapOptions =
  TapOptions
    { tapOptionsForce = Nothing
    , tapOptionsModifiers = Nothing
    , tapOptionsPosition = Nothing
    , tapOptionsTimeout = Nothing
    , tapOptionsTrial = Nothing
    }

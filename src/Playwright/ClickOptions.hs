module Playwright.ClickOptions
  ( ClickOptions (..)
  , defaultClickOptions
  ) where

import Playwright.KeyboardModifier
  ( KeyboardModifier
  )
import Playwright.MouseButton
  ( MouseButton
  )
import Playwright.Point
  ( Point
  )
import Playwright.Timeout
  ( Timeout
  )

data ClickOptions = ClickOptions
  { clickOptionsButton :: Maybe MouseButton
  , clickOptionsClickCount :: Maybe Int
  , clickOptionsForce :: Maybe Bool
  , clickOptionsModifiers :: Maybe [KeyboardModifier]
  , clickOptionsNoWaitAfter :: Maybe Bool
  , clickOptionsPosition :: Maybe Point
  , clickOptionsTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

defaultClickOptions :: ClickOptions
defaultClickOptions =
  ClickOptions
    { clickOptionsButton = Nothing
    , clickOptionsClickCount = Nothing
    , clickOptionsForce = Nothing
    , clickOptionsModifiers = Nothing
    , clickOptionsNoWaitAfter = Nothing
    , clickOptionsPosition = Nothing
    , clickOptionsTimeout = Nothing
    }

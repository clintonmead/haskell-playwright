module Playwright.DblclickOptions
  ( DblclickOptions (..)
  , defaultDblclickOptions
  ) where

import Playwright.KeyboardModifier
  ( KeyboardModifier
  )
import Playwright.Point
  ( Point
  )
import Playwright.ScrollOption
  ( ScrollOption
  )
import Playwright.Timeout
  ( Timeout
  )

data DblclickOptions = DblclickOptions
  { dblclickOptionsForce :: Maybe Bool
  , dblclickOptionsModifiers :: Maybe [KeyboardModifier]
  , dblclickOptionsPosition :: Maybe Point
  , dblclickOptionsScroll :: Maybe ScrollOption
  , dblclickOptionsTimeout :: Maybe Timeout
  , dblclickOptionsTrial :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultDblclickOptions :: DblclickOptions
defaultDblclickOptions =
  DblclickOptions
    { dblclickOptionsForce = Nothing
    , dblclickOptionsModifiers = Nothing
    , dblclickOptionsPosition = Nothing
    , dblclickOptionsScroll = Nothing
    , dblclickOptionsTimeout = Nothing
    , dblclickOptionsTrial = Nothing
    }

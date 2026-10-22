module Playwright.HoverOptions
  ( HoverOptions (..)
  , defaultHoverOptions
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

data HoverOptions = HoverOptions
  { hoverOptionsForce :: Maybe Bool
  , hoverOptionsModifiers :: Maybe [KeyboardModifier]
  , hoverOptionsPosition :: Maybe Point
  , hoverOptionsScroll :: Maybe ScrollOption
  , hoverOptionsTimeout :: Maybe Timeout
  , hoverOptionsTrial :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultHoverOptions :: HoverOptions
defaultHoverOptions =
  HoverOptions
    { hoverOptionsForce = Nothing
    , hoverOptionsModifiers = Nothing
    , hoverOptionsPosition = Nothing
    , hoverOptionsScroll = Nothing
    , hoverOptionsTimeout = Nothing
    , hoverOptionsTrial = Nothing
    }

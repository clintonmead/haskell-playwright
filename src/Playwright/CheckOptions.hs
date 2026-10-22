module Playwright.CheckOptions
  ( CheckOptions (..)
  , defaultCheckOptions
  ) where

import Playwright.Point
  ( Point
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )

data CheckOptions = CheckOptions
  { checkOptionsForce :: Maybe Bool
  , checkOptionsPosition :: Maybe Point
  , checkOptionsTimeout :: Timeout
  , checkOptionsTrial :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultCheckOptions :: CheckOptions
defaultCheckOptions =
  CheckOptions
    { checkOptionsForce = Nothing
    , checkOptionsPosition = Nothing
    , checkOptionsTimeout = defaultTimeout
    , checkOptionsTrial = Nothing
    }

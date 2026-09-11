module Playwright.CheckOptions
  ( CheckOptions (..)
  , defaultCheckOptions
  ) where

import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )

data CheckOptions = CheckOptions
  { checkOptionsForce :: Maybe Bool
  , checkOptionsTimeout :: Timeout
  , checkOptionsTrial :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultCheckOptions :: CheckOptions
defaultCheckOptions =
  CheckOptions
    { checkOptionsForce = Nothing
    , checkOptionsTimeout = defaultTimeout
    , checkOptionsTrial = Nothing
    }

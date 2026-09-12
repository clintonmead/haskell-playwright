module Playwright.ClickOptions
  ( ClickOptions (..)
  , defaultClickOptions
  ) where

import Playwright.Timeout
  ( Timeout
  )

data ClickOptions = ClickOptions
  { clickOptionsNoWaitAfter :: Maybe Bool
  , clickOptionsTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

defaultClickOptions :: ClickOptions
defaultClickOptions =
  ClickOptions
    { clickOptionsNoWaitAfter = Nothing
    , clickOptionsTimeout = Nothing
    }

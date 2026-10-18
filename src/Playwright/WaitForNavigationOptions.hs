module Playwright.WaitForNavigationOptions
  ( WaitForNavigationOptions (..)
  , defaultWaitForNavigationOptions
  ) where

import Playwright.LoadState
  ( LoadState
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.UrlGlob
  ( UrlGlob
  )

data WaitForNavigationOptions = WaitForNavigationOptions
  { waitForNavigationOptionsTimeout :: Maybe Timeout
  , waitForNavigationOptionsUrl :: Maybe UrlGlob
  , waitForNavigationOptionsWaitUntil :: Maybe LoadState
  }
  deriving stock (Eq, Show)

-- | Unset timeout uses the page navigation timeout. Unset waitUntil waits for
-- the load event. Unset url matches any navigation.
defaultWaitForNavigationOptions :: WaitForNavigationOptions
defaultWaitForNavigationOptions =
  WaitForNavigationOptions
    { waitForNavigationOptionsTimeout = Nothing
    , waitForNavigationOptionsUrl = Nothing
    , waitForNavigationOptionsWaitUntil = Nothing
    }

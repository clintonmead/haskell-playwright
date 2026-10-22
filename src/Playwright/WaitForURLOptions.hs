module Playwright.WaitForURLOptions
  ( WaitForURLOptions (..)
  , defaultWaitForURLOptions
  ) where

import Playwright.LoadState
  ( LoadState
  )
import Playwright.Timeout
  ( Timeout
  )

data WaitForURLOptions = WaitForURLOptions
  { waitForURLOptionsTimeout :: Maybe Timeout
  , waitForURLOptionsWaitUntil :: Maybe LoadState
  }
  deriving stock (Eq, Show)

-- | Unset timeout uses the page navigation timeout. Unset waitUntil waits for
-- the load event.
defaultWaitForURLOptions :: WaitForURLOptions
defaultWaitForURLOptions =
  WaitForURLOptions
    { waitForURLOptionsTimeout = Nothing
    , waitForURLOptionsWaitUntil = Nothing
    }

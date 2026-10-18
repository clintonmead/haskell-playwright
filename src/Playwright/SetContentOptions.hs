module Playwright.SetContentOptions
  ( SetContentOptions (..)
  , defaultSetContentOptions
  ) where

import Playwright.LoadState
  ( LoadState
  )
import Playwright.Timeout
  ( Timeout
  )

data SetContentOptions = SetContentOptions
  { setContentOptionsTimeout :: Maybe Timeout
  , setContentOptionsWaitUntil :: Maybe LoadState
  }
  deriving stock (Eq, Show)

-- | Unset timeout uses the page navigation timeout. Unset waitUntil waits for
-- the load event.
defaultSetContentOptions :: SetContentOptions
defaultSetContentOptions =
  SetContentOptions
    { setContentOptionsTimeout = Nothing
    , setContentOptionsWaitUntil = Nothing
    }

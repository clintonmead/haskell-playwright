module Playwright.ResourceTiming
  ( ResourceTiming (..)
  ) where

import Data.Scientific
  ( Scientific
  )

data ResourceTiming = ResourceTiming
  { resourceTimingStartTime :: Scientific
  , resourceTimingDomainLookupStart :: Scientific
  , resourceTimingDomainLookupEnd :: Scientific
  , resourceTimingConnectStart :: Scientific
  , resourceTimingSecureConnectionStart :: Scientific
  , resourceTimingConnectEnd :: Scientific
  , resourceTimingRequestStart :: Scientific
  , resourceTimingResponseStart :: Scientific
  , resourceTimingResponseEnd :: Scientific
  }
  deriving stock (Eq, Show)

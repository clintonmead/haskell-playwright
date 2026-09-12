module Playwright.EventWaiter
  ( EventWaiter (..)
  ) where

import Data.Aeson
  ( Value
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.MethodName
  ( MethodName
  )

data EventWaiter = EventWaiter
  { eventWaiterGuid :: Guid
  , eventWaiterMethod :: MethodName
  , eventWaiterValue :: Maybe Value
  }

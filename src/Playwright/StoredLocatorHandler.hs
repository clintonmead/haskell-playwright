module Playwright.StoredLocatorHandler
  ( StoredLocatorHandler (..)
  ) where

import Data.Text
  ( Text
  )
import Playwright.Guid
  ( Guid
  )

data StoredLocatorHandler = StoredLocatorHandler
  { storedLocatorHandlerTimes :: Maybe Int
  , storedLocatorHandlerSelector :: Text
  , storedLocatorHandlerFrame :: Guid
  , storedLocatorHandlerRun :: IO ()
  }

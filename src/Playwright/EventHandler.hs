module Playwright.EventHandler
  ( EventHandler (..)
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

data EventHandler = EventHandler
  { eventHandlerGuid :: Guid
  , eventHandlerMethod :: MethodName
  , eventHandlerRun :: Value -> IO ()
  }

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
  , -- Runs on the reader thread as the event is read, before any queued run.
    -- Only for registering handlers; it must not send a request.
    eventHandlerOnDispatch :: Value -> IO ()
  , eventHandlerRun :: Value -> IO ()
  }

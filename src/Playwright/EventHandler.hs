module Playwright.EventHandler
  ( EventHandler (..)
  , ListenerRemoval (..)
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
  , -- The owner a handler was registered for, so its listeners can be removed
    -- together. Nothing for handlers that cannot be removed individually.
    eventHandlerScope :: Maybe Guid
  , -- Runs on the reader thread as the event is read, before any queued run.
    -- Only for registering handlers; it must not send a request.
    eventHandlerOnDispatch :: Value -> IO ()
  , eventHandlerRun :: Value -> IO ()
  }

-- | How 'removeScopedEventHandlers' treats listeners that are still running.
data ListenerRemoval
  = -- | Remove the listeners and return at once.
    RemoveListeners
  | -- | Remove the listeners and drop any error they raise later.
    RemoveIgnoringErrors
  | -- | Remove the listeners, wait for the runs in flight, and rethrow the
    -- first error one of them raised.
    RemoveWaiting
  deriving stock (Eq, Show)

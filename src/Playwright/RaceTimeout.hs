module Playwright.RaceTimeout
  ( waitWithTimeout
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Concurrent.Async
  ( race
  )
import Control.Exception
  ( throwIO
  )
import Playwright.Error
  ( PlaywrightError (WaitTimeout)
  )
import Playwright.Timeout
  ( Timeout
  , timeoutToDouble
  )

waitWithTimeout :: Timeout -> IO a -> IO a
waitWithTimeout timeout action =
  if timeoutToDouble timeout == 0
    then action
    else do
      result <- race (threadDelay (round (timeoutToDouble timeout * 1000))) action
      case result of
        Left () ->
          throwIO (WaitTimeout timeout)
        Right value ->
          pure value

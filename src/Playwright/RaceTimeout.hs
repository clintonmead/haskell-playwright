module Playwright.RaceTimeout
  ( waitWithTimeout
  ) where

import Playwright.Error
  ( PlaywrightError (WaitTimeout)
  )
import Playwright.Timeout
  ( Timeout
  , timeoutToDouble
  )
import UnliftIO
  ( MonadUnliftIO
  )
import UnliftIO.Async
  ( race
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Exception
  ( throwIO
  )

waitWithTimeout :: (MonadUnliftIO m) => Timeout -> m a -> m a
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

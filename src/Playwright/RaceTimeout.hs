module Playwright.RaceTimeout
  ( waitWithTimeout
  , waitWithTimeoutLog
  ) where

import Data.Text
  ( Text
  )
import Playwright.Error
  ( PlaywrightError (WaitTimeout, WaitTimeoutLog)
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
waitWithTimeout timeout =
  raceWithTimeout (WaitTimeout timeout) timeout

-- | Like 'waitWithTimeout', but the timeout carries the call log line, such as
-- @waiting for response "foo.css"@, so the error says what was waited for.
waitWithTimeoutLog :: (MonadUnliftIO m) => Text -> Timeout -> m a -> m a
waitWithTimeoutLog logLine timeout =
  raceWithTimeout (WaitTimeoutLog timeout logLine) timeout

raceWithTimeout :: (MonadUnliftIO m) => PlaywrightError -> Timeout -> m a -> m a
raceWithTimeout err timeout action =
  if timeoutToDouble timeout == 0
    then action
    else do
      result <- race (threadDelay (round (timeoutToDouble timeout * 1000))) action
      case result of
        Left () ->
          throwIO err
        Right value ->
          pure value

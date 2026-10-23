module Playwright.Logger
  ( Logger (..)
  , LogSeverity (..)
  ) where

import Data.Text
  ( Text
  )

-- | How serious a log line is. Upstream calls these @verbose@, @info@,
-- @warning@ and @error@.
data LogSeverity
  = LogVerbose
  | LogInfo
  | LogWarning
  | LogError
  deriving stock (Eq, Show)

-- | A sink for the library's own log lines, as upstream's @logger@ option.
-- @loggerLog name severity message@ receives each line. The name is the
-- logging channel, such as @"api"@. @loggerIsEnabled name severity@ says
-- whether the library should bother to produce a line at all.
data Logger = Logger
  { loggerLog :: Text -> LogSeverity -> Text -> IO ()
  , loggerIsEnabled :: Text -> LogSeverity -> Bool
  }

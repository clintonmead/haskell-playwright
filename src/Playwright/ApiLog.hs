module Playwright.ApiLog
  ( withApiLog
  , withLoggerApi
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Control.Exception
  ( SomeException
  )
import Data.Text
  ( Text
  )
import Playwright.Connection
  ( lookupLogger
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Logger
  ( LogSeverity (LogInfo)
  , Logger (loggerIsEnabled, loggerLog)
  )
import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  , try
  )

-- | Runs an API call on the object it is made against, logging it to that
-- object's logger (inherited from its ancestors) when one is set.
withApiLog :: Guid -> Text -> Playwright a -> Playwright a
withApiLog guid name action = do
  logger <- lookupLogger guid
  withLoggerApi logger name action

-- | Runs an API call, logging @<name> started@ and then @<name> succeeded@ or
-- @<name> failed@ to the given logger, as upstream's API logging does.
withLoggerApi :: Maybe Logger -> Text -> Playwright a -> Playwright a
withLoggerApi Nothing _ action =
  action
withLoggerApi (Just logger) name action
  | loggerIsEnabled logger apiChannel LogInfo = do
      logLine "started"
      result <- try action
      case result of
        Right value -> do
          logLine "succeeded"
          pure value
        Left err -> do
          logLine "failed"
          throwIO (err :: SomeException)
  | otherwise =
      action
  where
    apiChannel :: Text
    apiChannel = "api"
    logLine :: Text -> Playwright ()
    logLine verb =
      liftIO (loggerLog logger apiChannel LogInfo (name <> " " <> verb))

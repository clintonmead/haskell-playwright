module Playwright.LoggerSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright
  ( LogSeverity (LogVerbose, LogInfo)
  , Logger (Logger, loggerIsEnabled, loggerLog)
  , chromium
  , click
  , closeBrowser
  , defaultLaunchOptions
  , defaultNewContextOptions
  , launch
  , launchWithLogger
  , locator
  , newContext
  , newContextWithLogger
  , newPage
  , runPlaywright
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( assertBool
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.STM
  ( TVar
  , atomically
  , newTVarIO
  , readTVar
  , readTVarIO
  , writeTVar
  )

type LogEntry = (Text, LogSeverity, Text)

tests :: TestTree
tests =
  testGroup
    "logger"
    [ testCase "should log @smoke" $ do
        logs <- newTVarIO []
        runPlaywright $ do
          browserType <- chromium
          browser <- launchWithLogger (recordingLogger logs) browserType defaultLaunchOptions
          _ <- newContext browser
          closeBrowser browser
        entries <- readTVarIO logs
        assertBool "an info line is logged" (any isInfo entries)
        assertBool
          "browser.newContext started is logged"
          (any (messageContains "browser.newContext started") entries)
        assertBool
          "browser.newContext succeeded is logged"
          (any (messageContains "browser.newContext succeeded") entries)
    , testCase "should log context-level" $ do
        logs <- newTVarIO []
        runPlaywright $ do
          browserType <- chromium
          browser <- launch browserType defaultLaunchOptions
          context <-
            newContextWithLogger
              (recordingLogger logs)
              browser
              defaultNewContextOptions
          page <- newPage context
          setContent page "<button>Button</button>"
          -- Haskell has no page.click, so the locator's click stands in for it.
          click (locator page (selectorFromText "button"))
          closeBrowser browser
        entries <- readTVarIO logs
        assertBool
          "page.setContent is logged"
          (any (messageContains "page.setContent") entries)
        assertBool
          "locator.click is logged"
          (any (messageContains "locator.click") entries)
    ]

-- | Collects every line into the given variable, and skips verbose lines the
-- same way upstream's @isEnabled@ does in the spec.
recordingLogger :: TVar [LogEntry] -> Logger
recordingLogger logs =
  Logger
    { loggerLog = \name severity message ->
        atomically $ do
          entries <- readTVar logs
          writeTVar logs ((name, severity, message) : entries)
    , loggerIsEnabled = \_ severity ->
        severity /= LogVerbose
    }

isInfo :: LogEntry -> Bool
isInfo (_, severity, _) =
  severity == LogInfo

messageContains :: Text -> LogEntry -> Bool
messageContains needle (_, _, message) =
  needle `Text.isInfixOf` message

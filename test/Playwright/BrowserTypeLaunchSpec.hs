module Playwright.BrowserTypeLaunchSpec
  ( tests
  ) where

import Playwright
  ( LaunchOptions (launchOptionsExecutablePath)
  , chromium
  , closeBrowser
  , defaultLaunchOptions
  , evaluate
  , launch
  , newContext
  , newPage
  , onContextClose
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumBrowser
  , assertEqual
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( concurrently_
  , wait
  , withAsync
  )
import UnliftIO.STM
  ( atomically
  , newTVarIO
  , readTVarIO
  , writeTVar
  )

tests :: TestTree
tests =
  testGroup
    "browsertype-launch"
    [ testCase "should reject all promises when browser is closed" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          page <- newPage ctx
          assertProtocolContains "closed" $
            withAsync (evaluate page "() => new Promise(() => {})") $ \pending -> do
              _ <- evaluate page "() => new Promise(f => setTimeout(f, 0))"
              closeBrowser browser
              wait pending
    , testCase "should reject if executable path is invalid" $
        withChromiumBrowser $ \_browser -> do
          browserType <- chromium
          assertProtocolContains
            "Failed to launch"
            ( launch
                browserType
                defaultLaunchOptions {launchOptionsExecutablePath = Just "random-invalid-path"}
            )
    , testCase "should fire close event for all contexts" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          closed <- newTVarIO False
          onContextClose ctx (atomically (writeTVar closed True))
          closeBrowser browser
          assertEqual "closed" True =<< readTVarIO closed
    , testCase "should be callable twice" $
        withChromiumBrowser $ \browser -> do
          concurrently_ (closeBrowser browser) (closeBrowser browser)
          closeBrowser browser
    ]

-- Official cases that stay skipped:
-- userDataDir / args / port (not on typed LaunchOptions; userDataDir belongs on launchPersistentContext)
-- __testHookBeforeCreateBrowser timeout / Dummy exception
-- `process` as options, `await using` (Node)
-- dummy_bad_browser_executable.js (same as invalid executablePath)

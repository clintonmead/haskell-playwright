module Playwright.LauncherSpec
  ( tests
  ) where

import Playwright
  ( LaunchOptions (launchOptionsHeadless)
  , chromium
  , defaultLaunchOptions
  , launch
  , runPlaywright
  )
import Playwright.Harness
  ( assertProtocolContainsAll
  )
import System.Environment
  ( lookupEnv
  , setEnv
  , unsetEnv
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( finally
  )

-- Skipped upstream cases (tests/library/launcher.spec.ts):
--   "should have an errors object": the Haskell client has no `playwright.errors`
--     namespace. Errors are PlaywrightError constructors.
--   "should have a devices object": the Haskell client has no device descriptor
--     registry (`playwright.devices`).
--   "should kill browser process on timeout after close": relies on the Node-only
--     `__testHookGracefullyClose` / `__testHookBrowserCloseTimeout` launch hooks.

tests :: TestTree
tests =
  testGroup
    "launcher"
    [ testCase "should throw a friendly error if its headed and there is no xserver on linux running" $ do
        -- The driver reads DISPLAY from its inherited environment, so the
        -- test removes it from the process before the driver starts. There
        -- is no launch env option in the Haskell client.
        saved <- lookupEnv "DISPLAY"
        unsetEnv "DISPLAY"
        ( runPlaywright $ do
            browserType <- chromium
            assertProtocolContainsAll
              [ "Looks like you launched a headed browser without having a XServer running."
              , "xvfb-run"
              ]
              ( launch
                  browserType
                  defaultLaunchOptions {launchOptionsHeadless = Just False}
              )
          )
          `finally` maybe (pure ()) (setEnv "DISPLAY") saved
    ]

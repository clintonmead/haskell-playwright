module Playwright.BrowserTypeLaunchSeleniumSpec
  ( tests
  ) where

import Playwright
  ( defaultLaunchOptions
  , firefox
  , launchWithSeleniumRemote
  , runPlaywright
  )
import Playwright.Harness
  ( assertProtocolContains
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "browsertype-launch-selenium"
    [ testCase "selenium grid 3.141.59 standalone chromium" $
        skipCase
          "needs java, selenium-server-standalone-3.141.59.jar (PWTEST_SELENIUM) and chromedriver (PWTEST_CHROMEDRIVER); the harness cannot start a Selenium grid yet"
    , testCase "selenium grid 3.141.59 hub + node chromium" $
        skipCase
          "needs java, selenium-server-standalone-3.141.59.jar (PWTEST_SELENIUM) and chromedriver (PWTEST_CHROMEDRIVER); the harness cannot start a Selenium hub and node yet"
    , testCase "selenium grid 4.8.3 standalone chromium" $
        skipCase
          "needs java, selenium-server-4.8.3.jar (PWTEST_SELENIUM) and chromedriver (PWTEST_CHROMEDRIVER); the harness cannot start a Selenium grid yet"
    , testCase "selenium grid 4.8.3 hub + node chromium" $
        skipCase
          "needs java, selenium-server-4.8.3.jar (PWTEST_SELENIUM) and chromedriver (PWTEST_CHROMEDRIVER); the harness cannot start a Selenium hub and node yet"
    , testCase "selenium grid 4.8.3 standalone chromium broken driver" $
        skipCase
          "needs java, selenium-server-4.8.3.jar (PWTEST_SELENIUM) and a running Selenium grid with the broken-selenium-driver.js asset; the harness cannot start one yet"
    , testCase "selenium grid 3.141.59 standalone non-chromium" $
        runPlaywright $ do
          browserType <- firefox
          assertProtocolContains
            "Connecting to SELENIUM_REMOTE_URL is only supported by Chromium"
            ( launchWithSeleniumRemote
                browserType
                "http://127.0.0.1:4444/wd/hub"
                defaultLaunchOptions
            )
    , testCase "selenium grid 3.141.59 standalone chromium through run-driver" $
        skipCase
          "uses upstream's in-process oop.start() JS API to run the driver with SELENIUM_REMOTE_URL; there is no Haskell equivalent, and it also needs a Selenium grid"
    ]

-- | Reports a case that cannot run in this harness yet, with the concrete
-- reason. Mirrors the repo's other skips, which print and return.
skipCase :: String -> IO ()
skipCase reason =
  putStrLn ("skipped: " <> reason)

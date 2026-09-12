module Playwright.BrowserTypeBasicSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( unpack
  )
import Playwright
  ( browserTypeExecutablePath
  , browserTypeName
  , chromium
  , runPlaywright
  )
import UnliftIO.Directory
  ( doesFileExist
  )
import Playwright.Harness
  ( assertEqual
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
    "browsertype-basic"
    [ testCase "browserType.executablePath should work" $
        runPlaywright $ do
          browserType <- chromium
          exists <- liftIO (doesFileExist (unpack (browserTypeExecutablePath browserType)))
          assertEqual "exists" True exists
    , testCase "browserType.name should work" $
        runPlaywright $ do
          browserType <- chromium
          assertEqual "chromium" "chromium" (browserTypeName browserType)
    ]

-- Official `should throw when trying to connect with not-chromium` skips Chromium.

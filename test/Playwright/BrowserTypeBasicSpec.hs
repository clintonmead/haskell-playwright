module Playwright.BrowserTypeBasicSpec
  ( tests
  ) where

import Data.Text
  ( unpack
  )
import Playwright
  ( browserTypeExecutablePath
  , browserTypeName
  , chromium
  , withPlaywright
  )
import System.Directory
  ( doesFileExist
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "browsertype-basic"
    [ testCase "browserType.executablePath should work" $
        withPlaywright $ \pw -> do
          exists <- doesFileExist (unpack (browserTypeExecutablePath (chromium pw)))
          assertEqual "exists" True exists
    , testCase "browserType.name should work" $
        withPlaywright $ \pw ->
          assertEqual "chromium" "chromium" (browserTypeName (chromium pw))
    ]

-- Official `should throw when trying to connect with not-chromium` skips Chromium.

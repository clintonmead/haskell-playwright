module Playwright.PageFileChooserSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Playwright
  ( armFileChooser
  , click
  , fileChooserMultiple
  , setContent
  )
import Playwright.Harness
  ( locate
  , withChromiumPage
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
    "page-filechooser"
    [ testCase "should emit event once" $
        withChromiumPage $ \page -> do
          setContent page "<input type=file>"
          takeChooser <- armFileChooser page
          withAsync takeChooser $ \handle -> do
            click (locate page "input")
            chooser <- wait handle
            assertEqual "single" False (fileChooserMultiple chooser)
    ]

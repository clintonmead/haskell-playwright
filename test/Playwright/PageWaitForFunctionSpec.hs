module Playwright.PageWaitForFunctionSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( concurrently_
  )
import GHC.Clock
  ( getMonotonicTimeNSec
  )
import Playwright
  ( evaluateBool
  , milliseconds
  , waitForFunction
  , waitForTimeout
  )
import Playwright.Harness
  ( withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-function"
    [ testCase "should timeout" $
        withChromiumPage $ \page -> do
          start <- getMonotonicTimeNSec
          case milliseconds 42 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              waitForTimeout page timeout
          end <- getMonotonicTimeNSec
          assertBool "waited at least half the timeout" (end - start >= 21000000)
    , testCase "should work when predicate is already true" $
        withChromiumPage $ \page -> do
          _ <- evaluateBool page "() => { window.__FOO = 1; return true; }"
          _ <- waitForFunction page "() => window.__FOO === 1"
          pure ()
    , testCase "should accept a function and wait" $
        withChromiumPage $ \page ->
          concurrently_
            (waitForFunction page "() => window.__FOO === 1")
            (evaluateBool page "() => { window.__FOO = 1; return true; }")
    ]

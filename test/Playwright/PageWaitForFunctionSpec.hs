module Playwright.PageWaitForFunctionSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Async
  ( concurrently_
  )
import GHC.Clock
  ( getMonotonicTimeNSec
  )
import Playwright
  ( evaluateBool
  , jsonValueNumber
  , milliseconds
  , setDefaultTimeout
  , waitForFunction
  , waitForFunctionWith
  , waitForTimeout
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumPage
  , assertBool
  , assertEqual
  , assertFailure
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
    "page-wait-for-function"
    [ testCase "should timeout" $
        withChromiumPage $ \page -> do
          start <- liftIO getMonotonicTimeNSec
          case milliseconds 42 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              waitForTimeout page timeout
          end <- liftIO getMonotonicTimeNSec
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
    , testCase "should return the success value as a JSHandle" $
        withChromiumPage $ \page -> do
          handle <- waitForFunction page "() => 5"
          assertEqual "5" (5 :: Double) =<< jsonValueNumber handle
    , testCase "should fail with predicate throwing on first call" $
        withChromiumPage $ \page ->
          assertProtocolContains "oh my" (waitForFunction page "() => { throw new Error('oh my'); }")
    , testCase "should respect timeout" $
        withChromiumPage $ \page ->
          case milliseconds 10 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertProtocolContains
                "Timeout 10ms exceeded"
                (waitForFunctionWith page "() => false" timeout)
    , testCase "should respect default timeout" $
        withChromiumPage $ \page ->
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertProtocolContains
                "Timeout 1ms exceeded"
                (waitForFunction page "() => false")
    ]

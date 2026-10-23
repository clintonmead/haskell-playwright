module Playwright.PageRequestGcSpec
  ( tests
  ) where

import Playwright
  ( SpecialJsValue (SpecialUndefined)
  , evaluate
  , evaluateBool
  , evaluateSpecial
  , requestGC
  )
import Playwright.Harness
  ( withChromiumPage
  , assertEqual
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
    "page-request-gc"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { globalThis.objectToDestroy = { hello: 'world' }; globalThis.weakRef = new WeakRef(globalThis.objectToDestroy); }"
          requestGC page
          assertEqual "alive" True
            =<< evaluateBool page "() => globalThis.weakRef.deref()?.hello === 'world'"
          requestGC page
          assertEqual "still alive" True
            =<< evaluateBool page "() => globalThis.weakRef.deref()?.hello === 'world'"
          _ <- evaluate page "() => { globalThis.objectToDestroy = null; }"
          requestGC page
          assertEqual "collected" SpecialUndefined
            =<< evaluateSpecial page "() => globalThis.weakRef.deref()"
    -- Skipped: "should collect element retained by locator hit-target interceptor after detach".
    -- Upstream marks it fixme on Chromium (issue 41575: the last hit-tested element stays referenced
    -- natively), and the Haskell spec runs on Chromium only, so there is no passing behaviour to port.
    -- "should collect element retained by locator hit-target interceptor after detach"
    ]

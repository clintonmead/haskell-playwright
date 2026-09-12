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
    ]

module Playwright.PageEventPopupSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Playwright
  ( evaluate
  , evaluateBool
  , opener
  , pagesEqual
  , waitForPopup
  )
import Playwright.Harness
  ( withChromiumPage
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
    "page-event-popup"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          withAsync (waitForPopup page) $ \handle -> do
            _ <- evaluate page "() => { window.__popup = window.open('about:blank'); }"
            popup <- wait handle
            assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
            assertEqual "opener on popup" True =<< evaluateBool popup "() => !!window.opener"
            matched <- opener popup
            case matched of
              Nothing ->
                assertFailure "expected opener"
              Just parent ->
                assertEqual "opener identity" True (pagesEqual parent page)
    ]

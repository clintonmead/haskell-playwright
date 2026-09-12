module Playwright.PageCloseSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( concurrently_
  , wait
  , withAsync
  )
import Data.Maybe
  ( isNothing
  )
import Playwright
  ( bringToFront
  , closePage
  , evaluate
  , evaluateText
  , isClosed
  , newPage
  , opener
  , setContent
  , waitForClose
  , waitForPopup
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumContext
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
    "page-close"
    [ testCase "should set the page close state" $
        withChromiumPage $ \page -> do
          assertEqual "open" False =<< isClosed page
          closePage page
          assertEqual "closed" True =<< isClosed page
    , testCase "should pass page to close event" $
        withChromiumPage $ \page ->
          withAsync (waitForClose page) $ \handle -> do
            closePage page
            wait handle
            assertEqual "closed" True =<< isClosed page
    , testCase "should be callable twice" $
        withChromiumPage $ \page -> do
          closePage page
          closePage page
    , testCase "should reject all promises when page is closed" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Target page, context or browser has been closed"
            ( concurrently_
                (evaluate page "() => new Promise(() => {})")
                (closePage page)
            )
    , testCase "should return null if parent page has been closed" $
        withChromiumPage $ \page ->
          withAsync (waitForPopup page) $ \handle -> do
            _ <- evaluate page "() => window.open('about:blank')"
            popup <- wait handle
            closePage page
            matched <- opener popup
            assertEqual "opener" True (isNothing matched)
    , testCase "Page.bringToFront should work" $
        withChromiumContext $ \ctx page1 -> do
          page2 <- newPage ctx
          setContent page1 "Page1"
          setContent page2 "Page2"
          bringToFront page1
          assertEqual "page1" "visible" =<< evaluateText page1 "() => document.visibilityState"
          assertEqual "page2" "visible" =<< evaluateText page2 "() => document.visibilityState"
          bringToFront page2
          assertEqual "page1 after" "visible" =<< evaluateText page1 "() => document.visibilityState"
          assertEqual "page2 after" "visible" =<< evaluateText page2 "() => document.visibilityState"
    ]

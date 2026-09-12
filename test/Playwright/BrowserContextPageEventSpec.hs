module Playwright.BrowserContextPageEventSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Playwright
  ( LoadState (LoadStateDomContentLoaded)
  , closePage
  , contextPages
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluate
  , evaluateText
  , evaluateWith
  , goto
  , newPage
  , opener
  , pagesEqual
  , querySelector
  , selectorFromText
  , textArgument
  , url
  , urlFromText
  , urlToText
  , waitForContextPage
  , waitForLoadState
  )
import Playwright.Harness
  ( withAssetsContextWith
  , withChromiumContext
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-page-event"
    [ testCase "should have url" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page ->
          withAsync (waitForContextPage ctx) $ \handle -> do
            _ <-
              evaluateWith
                page
                "url => window.open(url)"
                (textArgument (prefix <> "/empty.html"))
            other <- wait handle
            assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url other
    , testCase "should have url after domcontentloaded" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page ->
          withAsync (waitForContextPage ctx) $ \handle -> do
            _ <-
              evaluateWith
                page
                "url => window.open(url)"
                (textArgument (prefix <> "/empty.html"))
            other <- wait handle
            waitForLoadState other LoadStateDomContentLoaded
            assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url other
    , testCase "should have about:blank url with domcontentloaded" $
        withChromiumContext $ \ctx page ->
          withAsync (waitForContextPage ctx) $ \handle -> do
            _ <- evaluateWith page "url => window.open(url)" (textArgument "about:blank")
            other <- wait handle
            waitForLoadState other LoadStateDomContentLoaded
            assertEqual "url" "about:blank" . urlToText =<< url other
    , testCase "should have about:blank for empty url with domcontentloaded" $
        withChromiumContext $ \ctx page ->
          withAsync (waitForContextPage ctx) $ \handle -> do
            _ <- evaluate page "() => window.open()"
            other <- wait handle
            waitForLoadState other LoadStateDomContentLoaded
            assertEqual "url" "about:blank" . urlToText =<< url other
    , testCase "should report when a new page is created and closed" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page ->
          withAsync (waitForContextPage ctx) $ \handle -> do
            _ <-
              evaluateWith
                page
                "url => window.open(url)"
                (textArgument (prefix <> "/empty.html"))
            other <- wait handle
            assertEqual "hello" "Hello world" =<< evaluateText other "() => ['Hello', 'world'].join(' ')"
            body <- querySelector other (selectorFromText "body")
            case body of
              Nothing ->
                assertFailure "expected body"
              Just _ ->
                pure ()
            listed <- contextPages ctx
            assertEqual "has original" True (any (pagesEqual page) listed)
            assertEqual "has other" True (any (pagesEqual other) listed)
            closePage other
            after <- contextPages ctx
            assertEqual "other gone" False (any (pagesEqual other) after)
            assertEqual "original remains" True (any (pagesEqual page) after)
    , testCase "should report initialized pages" $
        withChromiumContext $ \ctx _ ->
          withAsync (waitForContextPage ctx) $ \handle -> do
            created <- newPage ctx
            waited <- wait handle
            assertEqual "same page" True (pagesEqual created waited)
            assertEqual "blank" "about:blank" . urlToText =<< url waited
            withAsync (waitForContextPage ctx) $ \popupHandle -> do
              _ <- evaluate created "() => window.open('about:blank')"
              popup <- wait popupHandle
              assertEqual "popup blank" "about:blank" . urlToText =<< url popup
    , testCase "should have an opener" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForContextPage ctx) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/popup/window-open.html")) defaultGotoOptions
            popup <- wait handle
            assertEqual "popup url" (prefix <> "/popup/popup.html") . urlToText =<< url popup
            matched <- opener popup
            case matched of
              Nothing ->
                assertFailure "expected opener"
              Just parent ->
                assertEqual "opener identity" True (pagesEqual parent page)
            none <- opener page
            case none of
              Nothing ->
                pure ()
              Just _ ->
                assertFailure "expected no opener on original page"
    ]

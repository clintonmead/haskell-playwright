module Playwright.PageWaitForUrlSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , LoadState (LoadStateCommit, LoadStateDomContentLoaded, LoadStateLoad)
  , UrlMatch (UrlMatchGlob, UrlMatchRegex)
  , WaitForURLOptions (waitForURLOptionsWaitUntil)
  , click
  , continueRoute
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultWaitForURLOptions
  , evaluate
  , evaluateOnFrameText
  , evaluateText
  , evaluateTextWith
  , frames
  , fulfillRoute
  , goto
  , milliseconds
  , route
  , setContent
  , textArgument
  , url
  , urlFromText
  , urlToText
  , waitForURL
  , waitForURLFrame
  , waitForURLOptions
  , waitForURLWith
  )
import Playwright.Harness
  ( assertWaitTimeout
  , locate
  , withAssetPage
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
  , assertStillPending
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
    "page-wait-for-url"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluateTextWith
              page
              "url => { window.location.href = url; return url; }"
              (textArgument (prefix <> "/grid.html"))
          waitForURL page (UrlMatchGlob "**/grid.html")
    , testCase "should respect timeout" $
        withAssetPage "empty.html" $ \page ->
          case milliseconds 2500 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertWaitTimeout (waitForURLWith page (UrlMatchGlob "**/frame.html") timeout)
    , testCase "should work with clicking on anchor links" $
        withAssetPage "empty.html" $ \page -> do
          setContent page "<a href='#foobar'>foobar</a>"
          click (locate page "a")
          waitForURL page (UrlMatchGlob "**/*#foobar")
    , testCase "should work with history.pushState()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "<a onclick='javascript:pushState()'>SPA</a><script>function pushState() { history.pushState({}, '', 'wow.html') }</script>"
          click (locate page "a")
          waitForURL page (UrlMatchGlob "**/wow.html")
          assertEqual "url" (prefix <> "/wow.html") . urlToText =<< url page
    , testCase "should work with history.replaceState()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "<a onclick='javascript:replaceState()'>SPA</a><script>function replaceState() { history.replaceState({}, '', '/replaced.html') }</script>"
          click (locate page "a")
          waitForURL page (UrlMatchGlob "**/replaced.html")
          assertEqual "url" (prefix <> "/replaced.html") . urlToText =<< url page
    , testCase "should work with DOM history.back()/history.forward()" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent
            page
            "<a id=back onclick='javascript:goBack()'>back</a><a id=forward onclick='javascript:goForward()'>forward</a><script>function goBack() { history.back(); } function goForward() { history.forward(); } history.pushState({}, '', '/first.html'); history.pushState({}, '', '/second.html');</script>"
          assertEqual "second" (prefix <> "/second.html") . urlToText =<< url page
          click (locate page "a#back")
          waitForURL page (UrlMatchGlob "**/first.html")
          assertEqual "first" (prefix <> "/first.html") . urlToText =<< url page
          click (locate page "a#forward")
          waitForURL page (UrlMatchGlob "**/second.html")
          assertEqual "second again" (prefix <> "/second.html") . urlToText =<< url page
    , testCase "should work with url match for same document navigations" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForURL page (UrlMatchRegex "third\\.html" "")) $ \pending -> do
            assertStillPending pending
            _ <- evaluate page "() => { history.pushState({}, '', '/first.html'); }"
            assertStillPending pending
            _ <- evaluate page "() => { history.pushState({}, '', '/second.html'); }"
            assertStillPending pending
            _ <- evaluate page "() => { history.pushState({}, '', '/third.html'); }"
            wait pending
    , testCase "should work with both domcontentloaded and load" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          held <- newEmptyMVar
          route page (UrlMatchGlob (prefix <> "/one-style.css")) $ \handled -> putMVar held handled
          withAsync (goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions) $ \navigation ->
            withAsync (waitForURLOptions page (UrlMatchGlob "**/one-style.html") (waitUntilOptions LoadStateDomContentLoaded)) $ \domContentLoaded ->
              withAsync (waitForURLOptions page (UrlMatchGlob "**/one-style.html") (waitUntilOptions LoadStateLoad)) $ \bothFired -> do
                handled <- takeMVar held
                wait domContentLoaded
                assertStillPending bothFired
                continueRoute handled
                wait bothFired
                _ <- wait navigation
                pure ()
    , testCase "should work with commit" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob (prefix <> "/script.js")) $ \_ -> pure ()
          route page (UrlMatchGlob (prefix <> "/empty.html")) $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText "<title>Hello</title><script src=\"script.js\"></script>")
                }
          withAsync (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) $ \_ -> do
            waitForURLOptions page (UrlMatchGlob "**/empty.html") (waitUntilOptions LoadStateCommit)
            assertEqual "title" "Hello" =<< evaluateText page "() => document.title"
    , testCase "should work with commit and about:blank" $
        withChromiumPage $ \page ->
          waitForURLOptions page (UrlMatchGlob "about:blank") (waitUntilOptions LoadStateCommit)
    , testCase "should work on frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          allFrames <- frames page
          case drop 1 allFrames of
            [] ->
              assertFailure "expected a child frame"
            child : _ -> do
              _ <- evaluateOnFrameText child ("() => { window.location.href = '" <> prefix <> "/grid.html'; return 'navigating'; }")
              waitForURLFrame child (UrlMatchGlob "**/grid.html")
    ]

waitUntilOptions :: LoadState -> WaitForURLOptions
waitUntilOptions state =
  defaultWaitForURLOptions {waitForURLOptionsWaitUntil = Just state}

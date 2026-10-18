module Playwright.PageEventPopupSpec
  ( tests
  ) where

import Data.Text qualified as Text
import Playwright
  ( Dialog (dialogMessage, dialogPage)
  , armDialog
  , armPopup
  , click
  , dismissDialog
  , defaultGotoOptions
  , evalOnSelector
  , evaluate
  , evaluateBool
  , evaluateOnFrame
  , evaluateWith
  , frames
  , goto
  , opener
  , pagesEqual
  , selectorFromText
  , setContent
  , textArgument
  , url
  , urlFromText
  , urlToText
  , waitForPopup
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , locate
  , withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
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
    , testCase "should work with window features" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForPopup page) $ \handle -> do
            _ <-
              evaluate
                page
                "() => { window.__popup = window.open(window.location.href, 'Title', 'toolbar=no,location=no,directories=no,status=no,menubar=no,scrollbars=yes,resizable=yes,width=780,height=200,top=0,left=0'); }"
            popup <- wait handle
            assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
            assertEqual "opener on popup" True =<< evaluateBool popup "() => !!window.opener"
    , testCase "should be able to capture alert" $
        withChromiumPage $ \page -> do
          takePopup <- armPopup page
          takeDialog <- armDialog page
          withAsync (evaluate page "() => { const win = window.open(''); win.alert('hello'); }") $ \pending -> do
            popup <- takePopup
            dialog <- takeDialog
            assertEqual "message" "hello" (dialogMessage dialog)
            assertEqual "dialog page" True
              (maybe False (pagesEqual popup) (dialogPage dialog))
            dismissDialog dialog
            _ <- wait pending
            pure ()
    , testCase "should work with empty url" $
        withChromiumPage $ \page -> do
          takePopup <- armPopup page
          _ <- evaluate page "() => { window.__popup = window.open(''); }"
          popup <- takePopup
          assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
          assertEqual "opener on popup" True =<< evaluateBool popup "() => !!window.opener"
    , testCase "should work with noopener and no url" $
        withChromiumPage $ \page -> do
          takePopup <- armPopup page
          _ <- evaluate page "() => { window.__popup = window.open(undefined, null, 'noopener'); }"
          popup <- takePopup
          -- Chromium reports `about:blank#blocked` here.
          popupUrl <- url popup
          assertEqual "url" "about:blank" (Text.takeWhile (/= '#') (urlToText popupUrl))
          assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
          assertEqual "opener on popup" False =<< evaluateBool popup "() => !!window.opener"
    , testCase "should work with noopener and about:blank" $
        withChromiumPage $ \page -> do
          takePopup <- armPopup page
          _ <- evaluate page "() => { window.__popup = window.open('about:blank', null, 'noopener'); }"
          popup <- takePopup
          assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
          assertEqual "opener on popup" False =<< evaluateBool popup "() => !!window.opener"
    , testCase "should work with noopener and url" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          takePopup <- armPopup page
          _ <-
            evaluateWith
              page
              "url => { window.__popup = window.open(url, null, 'noopener'); }"
              (textArgument (prefix <> "/empty.html"))
          popup <- takePopup
          assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
          assertEqual "opener on popup" False =<< evaluateBool popup "() => !!window.opener"
    , testCase "should work with clicking target=_blank" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a target=_blank rel=\"opener\" href=\"/one-style.html\">yo</a>"
          takePopup <- armPopup page
          click (locate page "a")
          popup <- takePopup
          assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
          assertEqual "opener on popup" True =<< evaluateBool popup "() => !!window.opener"
    , testCase "should work with fake-clicking target=_blank and rel=noopener" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a target=_blank rel=noopener href=\"/one-style.html\">yo</a>"
          takePopup <- armPopup page
          _ <- evalOnSelector page (selectorFromText "a") "a => a.click()"
          popup <- takePopup
          assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
          assertEqual "opener on popup" False =<< evaluateBool popup "() => !!window.opener"
    , testCase "should work with clicking target=_blank and rel=noopener" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a target=_blank rel=noopener href=\"/one-style.html\">yo</a>"
          takePopup <- armPopup page
          click (locate page "a")
          popup <- takePopup
          assertEqual "opener on original" False =<< evaluateBool page "() => !!window.opener"
          assertEqual "opener on popup" False =<< evaluateBool popup "() => !!window.opener"
    , testCase "should report popup opened from iframes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/two-frames.html")) defaultGotoOptions
          allFrames <- frames page
          frame <-
            case drop 1 allFrames of
              child : _ ->
                pure child
              [] ->
                assertFailure "expected a child frame"
          takePopup <- armPopup page
          _ <- evaluateOnFrame frame "() => { window.__popup = window.open(''); }"
          _ <- takePopup
          pure ()
    ]

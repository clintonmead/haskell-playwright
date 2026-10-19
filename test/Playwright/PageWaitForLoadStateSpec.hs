module Playwright.PageWaitForLoadStateSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( GotoOptions (gotoOptionsWaitUntil)
  , LoadState (LoadStateDomContentLoaded, LoadStateLoad, LoadStateNetworkIdle, LoadStateCommit)
  , Page
  , Playwright
  , SetContentOptions (setContentOptionsWaitUntil)
  , armLoadEvent
  , armPopup
  , closePage
  , click
  , defaultGotoOptions
  , defaultSetContentOptions
  , evaluate
  , evaluateText
  , goto
  , newPage
  , pageContext
  , setContent
  , setContentWith
  , url
  , urlFromText
  , urlToText
  , waitForLoadState
  , waitForLoadStateWith
  )
import Playwright.Harness
  ( assertEqual
  , assertFailure
  , locate
  , assertStillPending
  , assertWaitTimeout
  , rafraf
  , requireTimeout
  , stallingStylesheetApp
  , testAssetsDir
  , withAssetPage
  , withAssets
  , withChromiumPage
  , withLocalApp
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.Exception
  ( SomeException
  , try
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-load-state"
    [ testCase "should pick up ongoing navigation" $ do
        assets <- testAssetsDir
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stallingStylesheetApp assets arrived release) $ \prefix ->
            withAsync (goto page (urlFromText (prefix <> "/one-style.html")) domContentLoadedGoto) $ \navigation -> do
              takeMVar arrived
              withAsync (waitForLoadState page LoadStateLoad) $ \loaded -> do
                putMVar release ()
                wait loaded
              _ <- wait navigation
              pure ()
    , testCase "should respect timeout" $ do
        assets <- testAssetsDir
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stallingStylesheetApp assets arrived release) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/one-style.html")) domContentLoadedGoto
            timeout <- requireTimeout 1
            assertWaitTimeout (waitForLoadStateWith page LoadStateLoad timeout)
            putMVar release ()
    , testCase "should resolve immediately if loaded" $
        withAssetPage "one-style.html" $ \page ->
          waitForLoadState page LoadStateLoad
    , testCase "should resolve immediately if load state matches" $
        withAssetPage "empty.html" $ \page ->
          waitForLoadState page LoadStateDomContentLoaded
    , testCase "should work with pages that have loaded before being connected to" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          popup <- openPopup page "() => { window._popup = window.open(document.location.href); }"
          waitForLoadState popup LoadStateLoad
          current <- url popup
          assertEqual "url" (prefix <> "/empty.html") (urlToText current)
    , testCase "should wait for load state of empty url popup" $
        withChromiumPage $ \page -> do
          takePopup <- armPopup page
          readyState <-
            evaluateText page "() => { const popup = window.open(''); return popup.document.readyState; }"
          popup <- takePopup
          waitForLoadState popup LoadStateLoad
          assertEqual "ready state" "complete" readyState
          assertEqual "popup ready state" "complete" =<< evaluateText popup "() => document.readyState"
    , testCase "should wait for load state of about:blank popup" $
        withChromiumPage $ \page -> do
          popup <- openPopup page "() => { window.open('about:blank'); }"
          waitForLoadState popup LoadStateLoad
          assertEqual "ready state" "complete" =<< evaluateText popup "() => document.readyState"
    , testCase "should wait for load state of about:blank popup with noopener" $
        withChromiumPage $ \page -> do
          popup <- openPopup page "() => { window.open('about:blank', null, 'noopener'); }"
          waitForLoadState popup LoadStateLoad
          assertEqual "ready state" "complete" =<< evaluateText popup "() => document.readyState"
    , testCase "should wait for load state of popup with network url" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          popup <- openPopup page ("() => { window.open('" <> prefix <> "/empty.html'); }")
          waitForLoadState popup LoadStateLoad
          assertEqual "ready state" "complete" =<< evaluateText popup "() => document.readyState"
    , testCase "should wait for load state of popup with network url and noopener" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          popup <- openPopup page ("() => { window.open('" <> prefix <> "/empty.html', null, 'noopener'); }")
          waitForLoadState popup LoadStateLoad
          assertEqual "ready state" "complete" =<< evaluateText popup "() => document.readyState"
    , testCase "should work with clicking target=_blank" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a target=_blank rel=\"opener\" href=\"/one-style.html\">yo</a>"
          takePopup <- armPopup page
          click (locate page "a")
          popup <- takePopup
          waitForLoadState popup LoadStateLoad
          assertEqual "ready state" "complete" =<< evaluateText popup "() => document.readyState"
    , testCase "should resolve after popup load" $ do
        assets <- testAssetsDir
        arrived <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumPage $ \page ->
          withLocalApp (stallingStylesheetApp assets arrived release) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            popup <-
              openPopup
                page
                ("() => { window.popup = window.open('" <> prefix <> "/one-style.html'); }")
            takeMVar arrived
            withAsync (waitForLoadState popup LoadStateLoad) $ \loaded -> do
              rafraf page 5
              assertStillPending loaded
              putMVar release ()
              wait loaded
            current <- url popup
            assertEqual "url" (prefix <> "/one-style.html") (urlToText current)
    , testCase "should work with javascript: iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContentWith
            page
            "<iframe src=\"javascript:false\"></iframe>"
            defaultSetContentOptions {setContentOptionsWaitUntil = Just LoadStateCommit}
          waitForLoadState page LoadStateDomContentLoaded
          waitForLoadState page LoadStateLoad
          waitForLoadState page LoadStateNetworkIdle
    , testCase "should work with broken data-url iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContentWith
            page
            "<iframe src=\"data:text/html\"></iframe>"
            defaultSetContentOptions {setContentOptionsWaitUntil = Just LoadStateCommit}
          waitForLoadState page LoadStateDomContentLoaded
          waitForLoadState page LoadStateLoad
          waitForLoadState page LoadStateNetworkIdle
    , testCase "should wait for load state of newPage" $
        withChromiumPage $ \page -> do
          newPage' <- newPage (pageContext page)
          waitForLoadState newPage' LoadStateLoad
          assertEqual "ready state" "complete" =<< evaluateText newPage' "() => document.readyState"
    , testCase "armed load event fails promptly when the page closes" $
        withChromiumPage $ \page -> do
          takeLoad <- armLoadEvent page
          closePage page
          outcome <- race (liftIO (threadDelay 10000000)) (failsArmed takeLoad)
          case outcome of
            Left () ->
              assertFailure "armed load event hung after the page closed"
            Right failed ->
              assertEqual "failed" True failed
    ]

-- | True when the action throws. A hang shows up as the race timing out instead.
failsArmed :: Playwright () -> Playwright Bool
failsArmed action = do
  result <- try action :: Playwright (Either SomeException ())
  pure (either (const True) (const False) result)

-- | Navigates with waitUntil domcontentloaded, so the goto returns before the
-- stylesheet finishes.
domContentLoadedGoto :: GotoOptions
domContentLoadedGoto =
  defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateDomContentLoaded}

-- | Registers for the next popup, then runs the script that opens it.
openPopup :: Page -> Text -> Playwright Page
openPopup page script = do
  takePopup <- armPopup page
  _ <- evaluate page script
  takePopup

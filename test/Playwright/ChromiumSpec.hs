module Playwright.ChromiumSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import Data.Text
  ( replace
  )
import Playwright
  ( UrlGlob (UrlGlob)
  , contextRoute
  , continueRoute
  , defaultGotoOptions
  , evaluate
  , evaluateOnWorkerText
  , goto
  , onServiceWorker
  , pageContext
  , serviceWorkers
  , urlFromText
  , waitForServiceWorker
  , workersEqual
  )
import Playwright.Harness
  ( withAssets
  , assertBool
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
    "chromium"
    [ testCase "should create a worker from a service worker" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          withAsync (waitForServiceWorker ctx) $ \pending -> do
            _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
            worker <- wait pending
            assertEqual "scope" "[object ServiceWorkerGlobalScope]"
              =<< evaluateOnWorkerText worker "() => self.toString()"
    , testCase "should create a worker from service worker with noop routing" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          contextRoute ctx (UrlGlob "**/*") continueRoute
          withAsync (waitForServiceWorker ctx) $ \pending -> do
            _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
            worker <- wait pending
            assertEqual "scope" "[object ServiceWorkerGlobalScope]"
              =<< evaluateOnWorkerText worker "() => self.toString()"
    , testCase "serviceWorkers() should return current workers" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
            cross = replace "127.0.0.1" "localhost" prefix
          worker1 <-
            withAsync (waitForServiceWorker ctx) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
              wait pending
          listed1 <- serviceWorkers ctx
          assertEqual "one" 1 (length listed1)
          worker2 <-
            withAsync (waitForServiceWorker ctx) $ \pending -> do
              _ <- goto page (urlFromText (cross <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
              wait pending
          listed2 <- serviceWorkers ctx
          assertEqual "two" 2 (length listed2)
          assertBool "contains first" (any (workersEqual worker1) listed2)
          assertBool "contains second" (any (workersEqual worker2) listed2)
    , testCase "should not create a worker from a shared worker" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          created <- newIORef False
          onServiceWorker (pageContext page) (\_ -> writeIORef created True)
          _ <- evaluate page "() => { new SharedWorker('data:text/javascript,console.log(\"hi\")'); }"
          assertEqual "no service worker" False =<< readIORef created
    ]

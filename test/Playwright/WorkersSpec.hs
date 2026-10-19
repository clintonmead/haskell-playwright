module Playwright.WorkersSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( PageError (pageErrorMessage)
  , defaultGotoOptions
  , evaluate
  , evaluateOnWorkerNumber
  , evaluateOnWorkerText
  , goto
  , onWorkerClose
  , urlFromText
  , urlToText
  , waitForPageError
  , waitForWorker
  , workerUrl
  , workers
  )
import Playwright.Harness
  ( withAssetPage
  , withAssets
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
    "workers"
    [ testCase "Page.workers" $
        withAssets $ \prefix page ->
          withAsync (waitForWorker page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/worker/worker.html")) defaultGotoOptions
            worker <- wait handle
            url <- workerUrl worker
            assertBool "worker.js" ("worker.js" `isInfixOf` urlToText url)
            assertEqual "function" "worker function result" =<< evaluateOnWorkerText worker "() => self['workerFunction']()"
            listed <- workers page
            assertEqual "one worker" 1 (length listed)
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "gone" 0 . length =<< workers page
    , testCase "should report console logs" $
        withAssetPage "empty.html" $ \page -> do
          -- covered separately via page-event-console; keep a worker evaluate smoke test
          listed <- workers page
          assertEqual "none" 0 (length listed)
    , testCase "should evaluate" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1)'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          assertEqual "sum" (2 :: Double) =<< evaluateOnWorkerNumber worker "() => 1 + 1"
    , testCase "should clear upon navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1)'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          assertEqual "one worker" 1 . length =<< workers page
          destroyed <- newIORef False
          onWorkerClose worker (writeIORef destroyed True)
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          assertEqual "destroyed" True =<< readIORef destroyed
          assertEqual "gone" 0 . length =<< workers page
    , testCase "should emit created and destroyed events" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.__obj = new Worker(URL.createObjectURL(new Blob(['1'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          closed <- newEmptyMVar
          onWorkerClose worker (void (tryPutMVar closed ()))
          _ <- evaluate page "() => window.__obj.terminate()"
          -- Bounded: a close event that never arrives must fail the test, not hang the run.
          outcome <- race (liftIO (threadDelay 30000000)) (takeMVar closed)
          case outcome of
            Left () ->
              assertFailure "worker close event never arrived"
            Right () ->
              pure ()
    , testCase "should report errors" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob([\"setTimeout(() => { console.log('hey'); throw new Error('this is my error'); })\"], { type: 'application/javascript' }))); }"
          err <- waitForPageError page
          assertBool "message" ("this is my error" `isInfixOf` pageErrorMessage err)
    ]

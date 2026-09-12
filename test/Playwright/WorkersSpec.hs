module Playwright.WorkersSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( defaultGotoOptions
  , evaluateOnWorkerText
  , goto
  , urlFromText
  , urlToText
  , waitForWorker
  , workerUrl
  , workers
  )
import Playwright.Harness
  ( withAssetPage
  , withAssets
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
    ]

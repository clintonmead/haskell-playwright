module Playwright.SharedWorkerSpec
  ( tests
  ) where

import Playwright
  ( closePage
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , goto
  , newPage
  , urlFromText
  )
import Playwright.Harness
  ( withAssetsContextWith
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
    "shared-worker"
    [ testCase "should survive shared worker restart" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page1 -> do
          _ <-
            goto
              page1
              (urlFromText (prefix <> "/shared-worker/shared-worker.html"))
              defaultGotoOptions
          assertEqual "first" "echo:hello"
            =<< evaluateText page1 "() => window.sharedWorkerResponsePromise"
          closePage page1
          page2 <- newPage ctx
          _ <-
            goto
              page2
              (urlFromText (prefix <> "/shared-worker/shared-worker.html"))
              defaultGotoOptions
          assertEqual "second" "echo:hello"
            =<< evaluateText page2 "() => window.sharedWorkerResponsePromise"
          closePage page2
    ]

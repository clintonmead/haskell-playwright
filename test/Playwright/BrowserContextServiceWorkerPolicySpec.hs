module Playwright.BrowserContextServiceWorkerPolicySpec
  ( tests
  ) where

import Playwright
  ( NewContextOptions (newContextServiceWorkers)
  , ServiceWorkersPolicy (ServiceWorkersBlock)
  , armConsole
  , consoleMessageText
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateBool
  , goto
  , pageErrors
  , urlFromText
  )
import Playwright.Harness
  ( withAssetsWith
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
    "browsercontext-service-worker-policy"
    [ testCase "should allow service workers by default" $
        withAssetsWith defaultNewContextOptions $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
          assertEqual "registered" True
            =<< evaluateBool page "async () => !!(await window.registrationPromise)"
    , testCase "blocks service worker registration" $
        withAssetsWith
          defaultNewContextOptions {newContextServiceWorkers = Just ServiceWorkersBlock}
          $ \prefix page -> do
            takeMessage <- armConsole page
            _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
            message <- takeMessage
            assertEqual
              "blocked"
              "Service Worker registration blocked by Playwright"
              (consoleMessageText message)
    , testCase "should not throw error on about:blank" $
        withAssetsWith
          defaultNewContextOptions {newContextServiceWorkers = Just ServiceWorkersBlock}
          $ \_ page -> do
            _ <- goto page (urlFromText "about:blank") defaultGotoOptions
            errors <- pageErrors page
            assertEqual "none" [] errors
    ]

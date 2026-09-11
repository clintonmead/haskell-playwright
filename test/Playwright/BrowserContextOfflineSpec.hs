module Playwright.BrowserContextOfflineSpec
  ( tests
  ) where

import Playwright
  ( HttpStatus (httpStatusToInt)
  , NewContextOptions (newContextOffline)
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateBool
  , goto
  , pageContext
  , responseStatus
  , setOffline
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , withAssetsContextWith
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
    "browsercontext-offline"
    [ testCase "should emulate navigator.onLine" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            ctx = pageContext page
          assertEqual "online" True =<< evaluateBool page "() => window.navigator.onLine"
          setOffline ctx True
          assertEqual "offline" False =<< evaluateBool page "() => window.navigator.onLine"
          setOffline ctx False
          assertEqual "online again" True =<< evaluateBool page "() => window.navigator.onLine"
    , testCase "should work with offline option" $
        withAssetsContextWith defaultNewContextOptions {newContextOffline = Just True} $ \prefix ctx page -> do
          assertProtocolContains
            "ERR_INTERNET_DISCONNECTED"
            (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
          setOffline ctx False
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    ]

module Playwright.PageRequestContinueSpec
  ( tests
  ) where

import Playwright
  ( UrlGlob (UrlGlob)
  , continueRoute
  , defaultGotoOptions
  , goto
  , responseOk
  , route
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
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
    "page-request-continue"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/*") continueRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    ]

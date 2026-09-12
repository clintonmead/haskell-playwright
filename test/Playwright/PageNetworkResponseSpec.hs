module Playwright.PageNetworkResponseSpec
  ( tests
  ) where

import Playwright
  ( defaultGotoOptions
  , goto
  , responseText
  , urlFromText
  )
import Playwright.Harness
  ( withAssets
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
    "page-network-response"
    [ testCase "should return text" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/simple.json")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "{\"foo\": \"bar\"}\n" =<< responseText value
    ]

module Playwright.PageWaitForSelectorSpec
  ( tests
  ) where

import Playwright
  ( WaitForState (WaitForAttached)
  , defaultGotoOptions
  , evaluate
  , goto
  , milliseconds
  , selectorFromText
  , setContent
  , urlFromText
  , waitForSelector
  , waitForWith
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-selector"
    [ testCase "should immediately resolve promise if node exists" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          waitForSelector page (selectorFromText "*") WaitForAttached
          _ <- evaluate page "() => { document.body.appendChild(document.createElement('div')); }"
          waitForSelector page (selectorFromText "div") WaitForAttached
    , testCase "should timeout" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          case milliseconds 100 of
            Nothing ->
              assertFailure "milliseconds 100"
            Just timeout ->
              assertProtocolContains
                "Timeout 100ms exceeded"
                (waitForWith (locate page "span") timeout WaitForAttached)
    ]

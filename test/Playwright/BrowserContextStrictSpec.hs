module Playwright.BrowserContextStrictSpec
  ( tests
  ) where

import Playwright
  ( NewContextOptions (newContextStrictSelectors)
  , Page
  , defaultNewContextOptions
  , pageClickSelector
  , pageTextContent
  , pageTextContentWith
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumContextWith
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-strict"
    [ testCase "should not fail page.textContent in non-strict mode" $
        withChromiumPage $ \page -> do
          setContent page "<span>span1</span><div><span>target</span></div>"
          assertEqual "text" (Just "span1")
            =<< pageTextContentWith page (selectorFromText "span") (Just False)
    , testCase "should fail page.textContent in strict mode" $
        withStrictPage $ \page -> do
          setContent page "<span>span1</span><div><span>target</span></div>"
          assertProtocolContains
            "strict mode violation"
            (pageTextContent page (selectorFromText "span"))
    , testCase "should fail page.click in strict mode" $
        withStrictPage $ \page -> do
          setContent page "<button>button1</button><button>target</button>"
          assertProtocolContains
            "strict mode violation"
            (pageClickSelector page (selectorFromText "button"))
    , testCase "should opt out of strict mode" $
        withStrictPage $ \page -> do
          setContent page "<span>span1</span><div><span>target</span></div>"
          assertEqual "text" (Just "span1")
            =<< pageTextContentWith page (selectorFromText "span") (Just False)
    ]

withStrictPage :: (Page -> IO a) -> IO a
withStrictPage action =
  withChromiumContextWith
    defaultNewContextOptions {newContextStrictSelectors = Just True}
    (\_ page -> action page)

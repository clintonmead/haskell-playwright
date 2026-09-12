module Playwright.BrowserContextPagesSpec
  ( tests
  ) where

import Playwright
  ( closePage
  , contextPages
  , contextsEqual
  , pageContext
  , pagesEqual
  )
import Playwright.Harness
  ( withChromiumContext
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
    "browsercontext-pages"
    [ testCase "should not be visible in context.pages" $
        withChromiumContext $ \ctx page -> do
          listed <- contextPages ctx
          assertEqual "contains" True (any (pagesEqual page) listed)
          closePage page
          after <- contextPages ctx
          assertEqual "gone" False (any (pagesEqual page) after)
    , testCase "page.context should return the correct instance" $
        withChromiumContext $ \ctx page ->
          assertEqual "context" True (contextsEqual (pageContext page) ctx)
    ]

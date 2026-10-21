module Playwright.BrowserContextPagesSpec
  ( tests
  ) where

import Playwright
  ( NewContextOptions (newContextJavaScriptEnabled)
  , armNavigationOptions
  , click
  , closePage
  , defaultGotoOptions
  , defaultNewContextOptions
  , defaultWaitForNavigationOptions
  , evaluateText
  , focus
  , goto
  , newPage
  , setContent
  , url
  , urlFromText
  , urlToText
  , contextPages
  , contextsEqual
  , pageContext
  , pagesEqual
  )
import Playwright.Harness
  ( locate
  , withAssetsWith
  , withChromiumContext
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
    , testCase "frame.focus should work multiple times" $
        withChromiumContext $ \ctx page1 -> do
          page2 <- newPage ctx
          mapM_
            ( \page -> do
                setContent page "<button id=\"foo\" onfocus=\"window.gotFocus=true\"></button>"
                focus (locate page "#foo")
                assertEqual "focused" "true" =<< evaluateText page "() => String(!!window.gotFocus)"
            )
            [page1, page2]
    , testCase "should click with disabled javascript" $
        withAssetsWith defaultNewContextOptions {newContextJavaScriptEnabled = Just False} $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/wrappedlink.html")) defaultGotoOptions
          takeNavigation <- armNavigationOptions page defaultWaitForNavigationOptions
          click (locate page "a")
          _ <- takeNavigation
          assertEqual "url" (prefix <> "/wrappedlink.html#clicked") . urlToText =<< url page
    ]

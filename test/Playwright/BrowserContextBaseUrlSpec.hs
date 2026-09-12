module Playwright.BrowserContextBaseUrlSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( NewContextOptions (newContextBaseURL)
  , Page
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateText
  , goto
  , responseUrl
  , url
  , urlFromText
  , urlToText
  , Playwright
  )
import Playwright.Harness
  ( withAssetsPrefix
  , withChromiumContextWith
  , assertEqual
  , assertFailure
  )
import Playwright.Url
  ( resolveAgainstBase
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
    "browsercontext-base-url"
    [ testCase "resolves relative paths without a browser" $ do
        let
          prefix = urlFromText "http://example.com/url-construction"
          slashed = urlFromText "http://example.com/url-construction/"
        assertEqual
          "replace last segment"
          "http://example.com/mypage.html"
          (urlToText (resolveAgainstBase (Just prefix) (urlFromText "mypage.html")))
        assertEqual
          "dot slash"
          "http://example.com/mypage.html"
          (urlToText (resolveAgainstBase (Just prefix) (urlFromText "./mypage.html")))
        assertEqual
          "rooted"
          "http://example.com/mypage.html"
          (urlToText (resolveAgainstBase (Just prefix) (urlFromText "/mypage.html")))
        assertEqual
          "under directory"
          "http://example.com/url-construction/mypage.html"
          (urlToText (resolveAgainstBase (Just slashed) (urlFromText "mypage.html")))
        assertEqual
          "dot"
          "http://example.com/url-construction/"
          (urlToText (resolveAgainstBase (Just slashed) (urlFromText ".")))
        assertEqual
          "root slash"
          "http://example.com/"
          (urlToText (resolveAgainstBase (Just slashed) (urlFromText "/")))
        assertEqual
          "absolute unchanged"
          "https://playwright.dev/"
          (urlToText (resolveAgainstBase (Just slashed) (urlFromText "https://playwright.dev/")))
    , testCase "should construct a new URL when a baseURL in browser.newContext is passed to page.goto" $
        withAssetsPrefix $ \prefix ->
          withChromiumContextWith
            defaultNewContextOptions {newContextBaseURL = Just (urlFromText prefix)}
            $ \_ page ->
              expectGotoUrl page "/empty.html" (prefix <> "/empty.html")
    , testCase "should construct the URLs correctly when a baseURL without a trailing slash is passed to page.goto" $
        withAssetsPrefix $ \prefix ->
          withChromiumContextWith
            defaultNewContextOptions {newContextBaseURL = Just (urlFromText (prefix <> "/url-construction"))}
            $ \_ page -> do
              expectGotoUrl page "mypage.html" (prefix <> "/mypage.html")
              expectGotoUrl page "./mypage.html" (prefix <> "/mypage.html")
              expectGotoUrl page "/mypage.html" (prefix <> "/mypage.html")
    , testCase "should construct the URLs correctly when a baseURL with a trailing slash is passed to page.goto" $
        withAssetsPrefix $ \prefix ->
          withChromiumContextWith
            defaultNewContextOptions {newContextBaseURL = Just (urlFromText (prefix <> "/url-construction/"))}
            $ \_ page -> do
              expectGotoUrl page "mypage.html" (prefix <> "/url-construction/mypage.html")
              expectGotoUrl page "./mypage.html" (prefix <> "/url-construction/mypage.html")
              expectGotoUrl page "/mypage.html" (prefix <> "/mypage.html")
              expectGotoUrl page "." (prefix <> "/url-construction/")
              expectGotoUrl page "/" (prefix <> "/")
    , testCase "should not construct a new URL when valid URLs are passed" $
        withAssetsPrefix $ \prefix ->
          withChromiumContextWith
            defaultNewContextOptions {newContextBaseURL = Just (urlFromText "http://microsoft.com")}
            $ \_ page -> do
              expectGotoUrl page (prefix <> "/empty.html") (prefix <> "/empty.html")
              _ <- goto page (urlFromText "data:text/html,Hello world") defaultGotoOptions
              assertEqual "data" "data:text/html,Hello world" =<< evaluateText page "() => window.location.href"
              _ <- goto page (urlFromText "about:blank") defaultGotoOptions
              assertEqual "blank" "about:blank" =<< evaluateText page "() => window.location.href"
              current <- urlToText <$> url page
              assertEqual "page url" "about:blank" current
    ]

expectGotoUrl :: Page -> Text -> Text -> Playwright ()
expectGotoUrl page target expected = do
  response <- goto page (urlFromText target) defaultGotoOptions
  case response of
    Nothing ->
      assertFailure "expected navigation response"
    Just value ->
      assertEqual "response url" expected . urlToText =<< responseUrl value

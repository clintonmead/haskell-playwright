module Playwright.BrowserContextBaseUrlSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import UnliftIO.IORef
  ( newIORef
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )
import Playwright
  ( FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody)
  , NewContextOptions (newContextBaseURL)
  , Page
  , armRequest
  , armResponse
  , chromium
  , closeContext
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , fulfillRoute
  , launchPersistentContext
  , newContextWith
  , newPage
  , requestUrl
  , route
  , responseText
  , responseUrl
  , runPlaywright
  , UrlMatch (UrlMatchGlob)
  , waitForURL
  , evaluateText
  , goto
  , url
  , urlFromText
  , urlToText
  , Playwright
  )
import Playwright.Harness
  ( recordingPageApp
  , withChromiumBrowser
  , withLocalApp
  , withAssetsPrefix
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
    , testCase "should construct a new URL when a baseURL in browser.newPage is passed to page.goto" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            context <-
              newContextWith
                browser
                defaultNewContextOptions {newContextBaseURL = Just (urlFromText prefix)}
            page <- newPage context
            response <- goto page (urlFromText "/empty.html") defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value ->
                assertEqual "url" (prefix <> "/empty.html") . urlToText =<< responseUrl value
    , testCase "should construct the URLs correctly when a baseURL without a trailing slash in browser.newPage is passed to page.goto" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            context <-
              newContextWith browser defaultNewContextOptions {newContextBaseURL = Just (urlFromText (prefix <> "/url-construction"))}
            page <- newPage context
            assertEqual "relative" (prefix <> "/mypage.html") =<< landedAt page "mypage.html"
            assertEqual "dot" (prefix <> "/mypage.html") =<< landedAt page "./mypage.html"
            assertEqual "absolute" (prefix <> "/mypage.html") =<< landedAt page "/mypage.html"
    , testCase "should construct the URLs correctly when a baseURL with a trailing slash in browser.newPage is passed to page.goto" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            context <-
              newContextWith browser defaultNewContextOptions {newContextBaseURL = Just (urlFromText (prefix <> "/url-construction/"))}
            page <- newPage context
            assertEqual "relative" (prefix <> "/url-construction/mypage.html") =<< landedAt page "mypage.html"
            assertEqual "dot" (prefix <> "/url-construction/mypage.html") =<< landedAt page "./mypage.html"
            assertEqual "absolute" (prefix <> "/mypage.html") =<< landedAt page "/mypage.html"
            assertEqual "directory" (prefix <> "/url-construction/") =<< landedAt page "."
            assertEqual "root" (prefix <> "/") =<< landedAt page "/"
    , testCase "should be able to match a URL relative to its given URL with urlMatcher" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            context <-
              newContextWith browser defaultNewContextOptions {newContextBaseURL = Just (urlFromText (prefix <> "/foobar/"))}
            page <- newPage context
            _ <- goto page (urlFromText "/kek/index.html") defaultGotoOptions
            waitForURL page (UrlMatchGlob "/kek/index.html")
            assertEqual "page url" (prefix <> "/kek/index.html") . urlToText =<< url page
            route page (UrlMatchGlob "./kek/index.html") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillBody = Just (FulfillText "base-url-matched-route")
                  }
            awaitRequest <- armRequest page (UrlMatchGlob "./kek/index.html")
            awaitResponse <- armResponse page (UrlMatchGlob "./kek/index.html")
            _ <- goto page (urlFromText "./kek/index.html") defaultGotoOptions
            request <- awaitRequest
            response <- awaitResponse
            assertEqual "request url" (prefix <> "/foobar/kek/index.html") . urlToText =<< requestUrl request
            assertEqual "response url" (prefix <> "/foobar/kek/index.html") . urlToText =<< responseUrl response
            assertEqual "body" "base-url-matched-route" =<< responseText response
    , testCase "should not construct a new URL with baseURL when a glob was used" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            context <-
              newContextWith browser defaultNewContextOptions {newContextBaseURL = Just (urlFromText (prefix <> "/foobar/"))}
            page <- newPage context
            _ <- goto page (urlFromText "./kek/index.html") defaultGotoOptions
            waitForURL page (UrlMatchGlob "**/foobar/kek/index.html")
    , testCase "should construct a new URL when a baseURL in browserType.launchPersistentContext is passed to page.goto" $
        withAssetsPrefix $ \prefix ->
          withSystemTempDirectory "pw-user-data" $ \userDataDir ->
            runPlaywright $ do
              browserType <- chromium
              ctx <-
                launchPersistentContext
                  browserType
                  userDataDir
                  defaultLaunchOptions
                  defaultNewContextOptions {newContextBaseURL = Just (urlFromText prefix)}
              page <- newPage ctx
              expectGotoUrl page "/empty.html" (prefix <> "/empty.html")
              closeContext ctx
    ]

expectGotoUrl :: Page -> Text -> Text -> Playwright ()
expectGotoUrl page target expected = do
  response <- goto page (urlFromText target) defaultGotoOptions
  case response of
    Nothing ->
      assertFailure "expected navigation response"
    Just value ->
      assertEqual "response url" expected . urlToText =<< responseUrl value

-- | Navigates to the path, and returns the URL the page lands on.
landedAt :: Page -> Text -> Playwright Text
landedAt page path = do
  response <- goto page (urlFromText path) defaultGotoOptions
  case response of
    Nothing ->
      pure "missing response"
    Just value ->
      urlToText <$> responseUrl value

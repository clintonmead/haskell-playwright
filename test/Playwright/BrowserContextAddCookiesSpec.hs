module Playwright.BrowserContextAddCookiesSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Data.Text qualified as Text
import Playwright
  ( Cookie (cookieExpires, cookieHttpOnly, cookieName, cookiePath, cookieSecure, cookieValue)
  , CookieExpires (cookieExpiresToScientific)
  , CookieHttpOnly (cookieHttpOnlyToBool)
  , CookieDomain (CookieDomain)
  , CookieName (CookieName, cookieNameToText)
  , CookiePath (CookiePath, cookiePathToText)
  , CookieSecure (cookieSecureToBool)
  , CookieValue (CookieValue, cookieValueToText)
  , Playwright
  , addCookies
  , clearCookies
  , cookieForHost
  , cookieForUrl
  , cookies
  , defaultGotoOptions
  , evaluateOnFrameText
  , evaluateText
  , frames
  , goto
  , pageContext
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( Recorded (recordedHeaders, recordedPath)
  , assertBool
  , assertEqual
  , assertFailure
  , recordingPageApp
  , withAssets
  , withChromiumContext
  , withLocalApp
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( try
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-add-cookies"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "password") (CookieValue "123456")]
          assertEqual "document" "password=123456" =<< evaluateText page "() => document.cookie"
    , testCase "should round-trip a url cookie" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "password") (CookieValue "123456")]
          jar <- cookies (pageContext page)
          assertEqual "names" ["password"] (fmap (cookieNameToText . cookieName) jar)
    , testCase "should clear cookies" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "cookie1") (CookieValue "1")]
          assertEqual "before" "cookie1=1" =<< evaluateText page "() => document.cookie"
          clearCookies (pageContext page)
          assertEqual "cleared" [] =<< cookies (pageContext page)
    , testCase "should set multiple cookies" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [ cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "multiple-1") (CookieValue "123456")
            , cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "multiple-2") (CookieValue "bar")
            ]
          assertEqual "cookies" "multiple-1=123456;multiple-2=bar"
            =<< evaluateText page "() => document.cookie.split(';').map(cookie => cookie.trim()).sort().join(';')"
    , testCase "should set cookie with reasonable defaults" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "defaults") (CookieValue "123456")]
          jar <- cookies (pageContext page)
          assertEqual "names" ["defaults"] (fmap (cookieNameToText . cookieName) jar)
          assertEqual "values" ["123456"] (fmap (cookieValueToText . cookieValue) jar)
          assertEqual "paths" ["/"] (fmap (cookiePathToText . cookiePath) jar)
          assertEqual "http only" [False] (fmap (cookieHttpOnlyToBool . cookieHttpOnly) jar)
          assertEqual "secure" [False] (fmap (cookieSecureToBool . cookieSecure) jar)
    , testCase "should have expires set to -1 for session cookies" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "expires") (CookieValue "123456")]
          jar <- cookies (pageContext page)
          assertEqual "expires" [-1] (fmap (cookieExpiresToScientific . cookieExpires) jar)
    , testCase "should send cookie header" $ do
        requests <- newIORef []
        withChromiumContext $ \ctx page ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            addCookies
              ctx
              [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "cookie") (CookieValue "value")]
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- readIORef requests
            case filter ((== "/empty.html") . recordedPath) seen of
              latest : _ ->
                assertEqual "cookie header" (Just "cookie=value") (lookup "cookie" (recordedHeaders latest))
              [] ->
                assertFailure "the server saw no request for empty.html"
    , testCase "should not set a cookie with blank page URL" $
        withChromiumContext $ \ctx _ -> do
          result <-
            try (addCookies ctx [cookieForUrl (urlFromText "about:blank") (CookieName "example-cookie-blank") (CookieValue "best")])
              :: Playwright (Either PlaywrightError ())
          case result of
            Left (ProtocolRpc failure) ->
              assertBool "blank page" ("Blank page can not have cookie" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected the cookie to be refused"
    , testCase "should not set a cookie on a data URL page" $
        withChromiumContext $ \ctx _ -> do
          result <-
            try (addCookies ctx [cookieForUrl (urlFromText "data:,Hello%2C%20World!") (CookieName "example-cookie") (CookieValue "best")])
              :: Playwright (Either PlaywrightError ())
          case result of
            Left (ProtocolRpc failure) ->
              assertBool "data URL page" ("Data URL page can not have cookie \"example-cookie\"" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected the cookie to be refused"
    , testCase "should set cookies for a frame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          addCookies (pageContext page) [cookieForUrl (urlFromText prefix) (CookieName "frame-cookie") (CookieValue "value")]
          _ <-
            evaluateText
              page
              ( "async () => { const iframe = document.createElement('iframe'); document.body.appendChild(iframe); await new Promise(resolve => { iframe.onload = resolve; iframe.src = '"
                  <> prefix
                  <> "/grid.html'; }); return 'loaded'; }"
              )
          allFrames <- frames page
          case drop 1 allFrames of
            frame : _ ->
              assertEqual "frame cookie" "frame-cookie=value" =<< evaluateOnFrameText frame "() => document.cookie"
            [] ->
              assertFailure "expected a child frame"
    , testCase "should set a cookie with a path" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          addCookies
            (pageContext page)
            [cookieForHost (CookieDomain (Text.drop 7 prefix)) (CookiePath "/grid.html") (CookieName "gridcookie") (CookieValue "GRID")]
          jar <- cookies (pageContext page)
          assertEqual "path" ["/grid.html"] (fmap (cookiePathToText . cookiePath) jar)
          assertEqual "on grid" "gridcookie=GRID" =<< evaluateText page "() => document.cookie"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "off grid" "" =<< evaluateText page "() => document.cookie"
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          assertEqual "back on grid" "gridcookie=GRID" =<< evaluateText page "() => document.cookie"
    ]

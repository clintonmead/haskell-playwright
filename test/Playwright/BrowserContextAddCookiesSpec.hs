module Playwright.BrowserContextAddCookiesSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Data.Text qualified as Text
import Data.ByteString.Char8 qualified as BS8
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
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
  , newPage
  , newContext
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
  , withChromiumBrowser
  , recordRequest
  , withChromiumContext
  , withLocalApp
  )
import UnliftIO.IORef
  ( IORef
  , newIORef
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
    , testCase "should isolate cookies in browser contexts" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            ctx <- newContext browser
            other <- newContext browser
            addCookies ctx [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "isolatecookie") (CookieValue "page1value")]
            addCookies other [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "isolatecookie") (CookieValue "page2value")]
            jar1 <- cookies ctx
            jar2 <- cookies other
            assertEqual "lengths" [1, 1] [length jar1, length jar2]
            assertEqual "values" ["page1value", "page2value"] (fmap (cookieValueToText . cookieValue) jar1 <> fmap (cookieValueToText . cookieValue) jar2)
    , testCase "should isolate session cookies" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          page <- newPage ctx
          withLocalApp (setCookieApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/setsession.html")) defaultGotoOptions
            jar1 <- cookies ctx
            assertEqual "session" ["value"] (fmap (cookieValueToText . cookieValue) jar1)
            other <- newContext browser
            otherPage <- newPage other
            _ <- goto otherPage (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            jar2 <- cookies other
            assertEqual "isolated" 0 (length jar2)
    , testCase "should isolate persistent cookies" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          page <- newPage ctx
          withLocalApp (setCookieApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/setpersistent.html")) defaultGotoOptions
            jar1 <- cookies ctx
            assertEqual "persistent" ["persistent"] (fmap (cookieNameToText . cookieName) jar1)
            assertEqual "persistent value" ["persistent-value"] (fmap (cookieValueToText . cookieValue) jar1)
            other <- newContext browser
            otherPage <- newPage other
            _ <- goto otherPage (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            jar2 <- cookies other
            assertEqual "isolated" 0 (length jar2)
    , testCase "should isolate send cookie header" $ do
        requests <- newIORef []
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          withLocalApp (setCookieApp requests) $ \prefix -> do
            addCookies ctx [cookieForUrl (urlFromText (prefix <> "/empty.html")) (CookieName "sendcookie") (CookieValue "value")]
            page <- newPage ctx
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            firstSeen <- readIORef requests
            assertEqual "sent" (Just "sendcookie=value") . lookup "cookie" . recordedHeaders =<< latestFor "/empty.html" firstSeen
            other <- newContext browser
            otherPage <- newPage other
            _ <- goto otherPage (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            secondSeen <- readIORef requests
            assertEqual "not sent" Nothing . lookup "cookie" . recordedHeaders =<< latestFor "/empty.html" secondSeen
    ]

-- | Records every request. Sets a session cookie on /setsession.html and a
-- persistent cookie on /setpersistent.html, and answers every path with an
-- empty page.
setCookieApp :: IORef [Recorded] -> Application
setCookieApp requests request respond = do
  recordRequest requests request
  case pathInfo request of
    ["setsession.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html"), ("Set-Cookie", "session=value")] "")
    ["setpersistent.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html"), ("Set-Cookie", "persistent=persistent-value; max-age=3600")] "")
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")

-- | The most recent request to the path, if any.
latestFor :: BS8.ByteString -> [Recorded] -> Playwright Recorded
latestFor path seen =
  case filter ((== path) . recordedPath) seen of
    latest : _ -> pure latest
    [] -> assertFailure ("the server saw no request for " <> show path)

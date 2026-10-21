module Playwright.DefaultBrowserContext1Spec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List
  ( find
  )
import Data.Text
  ( Text
  , pack
  , toLower
  )
import Network.HTTP.Types
  ( hAuthorization
  , hContentType
  , status200
  , status401
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Cookie (cookieDomain, cookieExpires, cookieHttpOnly, cookieName, cookiePath, cookieSameSite, cookieSecure, cookieValue)
  , CookieDomain (cookieDomainToText)
  , CookieExpires (cookieExpiresToScientific)
  , CookieHttpOnly (cookieHttpOnlyToBool)
  , CookieName (CookieName, cookieNameToText)
  , CookiePath (cookiePathToText)
  , CookieSameSite (CookieSameSiteLax)
  , CookieSecure (cookieSecureToBool)
  , CookieToSet (cookieToSetSameSite)
  , CookieValue (CookieValue, cookieValueToText)
  , HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HttpPassword (HttpPassword)
  , HttpStatus (httpStatusToInt)
  , HttpUsername (HttpUsername)
  , NewContextOptions (newContextBypassCSP, newContextDeviceScaleFactor, newContextHttpCredentials, newContextJavaScriptEnabled, newContextOffline, newContextUserAgent, newContextViewport)
  , Playwright
  , ScriptTag (scriptTagContent)
  , UrlMatch (UrlMatchGlob)
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , addCookies
  , addScriptTag
  , clearCookies
  , click
  , cookieForUrl
  , cookies
  , defaultGotoOptions
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , downloadPath
  , emptyScriptTag
  , evaluateBool
  , evaluateNumber
  , evaluateText
  , goto
  , newPage
  , reload
  , requestHeaders
  , responseStatus
  , sessionExpires
  , setContent
  , urlFromText
  , waitForDownload
  , waitForRequest
  )
import Playwright.Harness
  ( locate
  , withAssetsPrefix
  , withPersistentContext
  , withPersistentContextWith
  , assertEqual
  , assertFailure
  , assertProtocolContains
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Directory
  ( doesFileExist
  )

tests :: TestTree
tests =
  testGroup
    "defaultbrowsercontext-1"
    [ testCase "context.cookies() should work" $
        withAssetsPrefix $ \prefix ->
          withPersistentContext $ \ctx page -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            documentCookie <-
              evaluateText
                page
                "() => { document.cookie = 'username=John Doe'; return document.cookie; }"
            assertEqual "document" "username=John Doe" documentCookie
            jar <- cookies ctx
            assertEqual "count" 1 (length jar)
            case jar of
              [cookie] ->
                assertSessionCookie cookie "username" "John Doe"
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "context.addCookies() should work" $
        withAssetsPrefix $ \prefix ->
          withPersistentContext $ \ctx page -> do
            let
              empty = urlFromText (prefix <> "/empty.html")
            _ <- goto page empty defaultGotoOptions
            addCookies
              ctx
              [ (cookieForUrl empty (CookieName "username") (CookieValue "John Doe"))
                  { cookieToSetSameSite = Just CookieSameSiteLax }
              ]
            assertEqual "document" "username=John Doe" =<< evaluateText page "() => document.cookie"
            jar <- cookies ctx
            case jar of
              [cookie] ->
                assertSessionCookie cookie "username" "John Doe"
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "context.clearCookies() should work" $
        withAssetsPrefix $ \prefix ->
          withPersistentContext $ \ctx page -> do
            let
              empty = urlFromText (prefix <> "/empty.html")
            _ <- goto page empty defaultGotoOptions
            addCookies
              ctx
              [ cookieForUrl empty (CookieName "cookie1") (CookieValue "1")
              , cookieForUrl empty (CookieName "cookie2") (CookieValue "2")
              ]
            assertEqual "before" "cookie1=1; cookie2=2" =<< evaluateText page "() => document.cookie"
            clearCookies ctx
            _ <- reload page
            assertEqual "cleared" [] =<< cookies ctx
            assertEqual "document" "" =<< evaluateText page "() => document.cookie"
    , testCase "should support viewport option" $
        withPersistentContextWith
          defaultNewContextOptions
            { newContextViewport =
                Just (ViewportSize (ViewportWidth 456) (ViewportHeight 789))
            }
          $ \ctx page -> do
            assertEqual "width" (456 :: Double) =<< evaluateNumber page "() => window.innerWidth"
            assertEqual "height" (789 :: Double) =<< evaluateNumber page "() => window.innerHeight"
            page2 <- newPage ctx
            assertEqual "page2 width" (456 :: Double) =<< evaluateNumber page2 "() => window.innerWidth"
            assertEqual "page2 height" (789 :: Double) =<< evaluateNumber page2 "() => window.innerHeight"
    , testCase "should support deviceScaleFactor option" $
        withPersistentContextWith
          defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 3)}
          $ \_ page ->
            assertEqual "ratio" (3 :: Double)
              =<< evaluateNumber page "() => window.devicePixelRatio"
    , testCase "should support userAgent option" $
        withAssetsPrefix $ \prefix ->
          withPersistentContextWith
            defaultNewContextOptions {newContextUserAgent = Just (UserAgent "foobar")}
            $ \_ page -> do
              assertEqual "ua" "foobar" =<< evaluateText page "() => navigator.userAgent"
              withAsync (waitForRequest page (UrlMatchGlob "**/empty.html")) $ \pending -> do
                _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
                request <- wait pending
                headers <- requestHeaders request
                assertEqual "header" (Just "foobar") (lookupHeader "user-agent" headers)
    , testCase "should support bypassCSP option" $
        withAssetsPrefix $ \prefix ->
          withPersistentContextWith
            defaultNewContextOptions {newContextBypassCSP = Just True}
            $ \_ page -> do
              _ <- goto page (urlFromText (prefix <> "/csp.html")) defaultGotoOptions
              _ <-
                addScriptTag
                  page
                  emptyScriptTag {scriptTagContent = Just "window[\"__injected\"] = 42;"}
              assertEqual "injected" (42 :: Double)
                =<< evaluateNumber page "() => window.__injected"
              assertEqual "inline" True
                =<< evaluateBool page "() => window.__inlineScriptValue === 42"
    , testCase "should support javascriptEnabled option" $
        withPersistentContextWith
          defaultNewContextOptions {newContextJavaScriptEnabled = Just False}
          $ \_ page -> do
            _ <-
              goto
                page
                (urlFromText "data:text/html, <script>var something = \"forbidden\"</script>")
                defaultGotoOptions
            assertProtocolContains
              "something is not defined"
              (evaluateText page "() => something")
    , testCase "should support httpCredentials option" $
        withApplication (pure (basicAuthApp "Basic dXNlcjpwYXNz")) $ \port ->
          withPersistentContextWith
            defaultNewContextOptions {newContextHttpCredentials = Just validCredentials}
            $ \_ page -> do
              response <-
                goto
                  page
                  (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/playground.html"))
                  defaultGotoOptions
              case response of
                Nothing ->
                  assertFailure "missing response"
                Just value ->
                  assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    , testCase "should support offline option" $
        withAssetsPrefix $ \prefix ->
          withPersistentContextWith
            defaultNewContextOptions {newContextOffline = Just True}
            $ \_ page ->
              assertProtocolContains
                "ERR_INTERNET_DISCONNECTED"
                (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should support acceptDownloads option" $
        withApplication (pure downloadApp) $ \port ->
          withPersistentContext $ \_ page -> do
            let
              prefix = "http://127.0.0.1:" <> pack (show port)
            setContent page ("<a href=\"" <> prefix <> "/download\">download</a>")
            withAsync (waitForDownload page) $ \handle -> do
              click (locate page "a")
              download <- wait handle
              path <- downloadPath download
              exists <- doesFileExist path
              assertEqual "exists" True exists
              contents <- liftIO (BS8.readFile path)
              assertEqual "body" (BS8.pack "Hello world") contents
    ]

assertSessionCookie :: Cookie -> Text -> Text -> Playwright ()
assertSessionCookie cookie name value = do
  assertEqual "name" name (cookieNameToText (cookieName cookie))
  assertEqual "value" value (cookieValueToText (cookieValue cookie))
  assertEqual "domain" "127.0.0.1" (cookieDomainToText (cookieDomain cookie))
  assertEqual "path" "/" (cookiePathToText (cookiePath cookie))
  assertEqual "expires" (cookieExpiresToScientific sessionExpires) (cookieExpiresToScientific (cookieExpires cookie))
  assertEqual "httpOnly" False (cookieHttpOnlyToBool (cookieHttpOnly cookie))
  assertEqual "secure" False (cookieSecureToBool (cookieSecure cookie))
  assertEqual "sameSite" CookieSameSiteLax (cookieSameSite cookie)

lookupHeader :: Text -> [HttpHeader] -> Maybe Text
lookupHeader name headers =
  fmap (headerValueToText . httpHeaderValue)
    ( find
        (\header -> toLower (headerNameToText (httpHeaderName header)) == toLower name)
        headers
    )

validCredentials :: HttpCredentials
validCredentials =
  HttpCredentials
    { httpCredentialsUsername = HttpUsername "user"
    , httpCredentialsPassword = HttpPassword "pass"
    , httpCredentialsOrigin = Nothing
    , httpCredentialsSend = Nothing
    }

basicAuthApp :: BS8.ByteString -> Application
basicAuthApp expected req respond =
  case lookup hAuthorization (Wai.requestHeaders req) of
    Just value
      | value == expected ->
          respond
            ( responseLBS
                status200
                [(hContentType, BS8.pack "text/html")]
                (LBS8.pack "<html><title>ok</title></html>")
            )
    _ ->
      respond
        ( responseLBS
            status401
            [("WWW-Authenticate", BS8.pack "Basic realm=\"test\"")]
            LBS8.empty
        )

downloadApp :: Application
downloadApp _ respond =
  respond $
    responseLBS
      status200
      [ (hContentType, "application/octet-stream")
      , ("Content-Disposition", "attachment")
      ]
      (LBS8.pack "Hello world")

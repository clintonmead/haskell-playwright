module Playwright.BrowserContextCookiesSpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List
  ( sort
  , sortOn
  )
import UnliftIO.IORef
  ( IORef
  , newIORef

  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Cookie (cookieDomain, cookieExpires, cookieHttpOnly, cookieName, cookiePath, cookieSameSite, cookieSecure, cookieValue)
  , CookieDomain (CookieDomain, cookieDomainToText)
  , CookieSecure (CookieSecure, cookieSecureToBool)
  , CookieToSet
      ( CookieToSet
      , cookieToSetDomain
      , cookieToSetExpires
      , cookieToSetHttpOnly
      , cookieToSetName
      , cookieToSetPath
      , cookieToSetSameSite
      , cookieToSetSecure
      , cookieToSetUrl
      , cookieToSetValue
      )
  , CookiePath (CookiePath, cookiePathToText)
  , cookieForHost
  , cookieForUrl
  , addCookies
  , pageContext
  , CookieHttpOnly (cookieHttpOnlyToBool)
  , CookieName (CookieName, cookieNameToText)
  , CookieSameSite (CookieSameSiteLax, CookieSameSiteNone, CookieSameSiteStrict)
  , CookieValue (CookieValue, cookieValueToText)
  , CookieExpires (CookieExpires, cookieExpiresToScientific)
  , cookiesFor
  , cookies
  , ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillHeaders)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , UrlMatch (UrlMatchGlob)
  , childLocator
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluateNumber
  , evaluateText
  , frameLocator
  , fulfillRoute
  , goto
  , route
  , selectorFromText
  , textContent
  , urlFromText
  )
import Playwright.Harness
  ( Recorded
  , recordRequest
  , assertBool
  , assertProtocolContains
  , locate
  , withChromiumPage
  , withLocalApp
  , withAssets
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
    "browsercontext-cookies"
    [ testCase "should return no cookies in pristine browser context" $
        withChromiumContext $ \ctx _page ->
          assertEqual "empty" [] =<< cookies ctx
    , testCase "should get a cookie" $
        withApplication (pure emptyHtml) $ \port ->
          withChromiumContext $ \ctx page -> do
            let prefix = "http://127.0.0.1:" <> pack (show port)
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            documentCookie <- evaluateText page "() => { document.cookie = 'username=John Doe'; return document.cookie; }"
            assertEqual "document" "username=John Doe" documentCookie
            jar <- cookies ctx
            assertEqual "count" 1 (length jar)
            case jar of
              [cookie] -> do
                assertEqual "name" "username" (cookieNameToText (cookieName cookie))
                assertEqual "value" "John Doe" (cookieValueToText (cookieValue cookie))
                assertEqual "httpOnly" False (cookieHttpOnlyToBool (cookieHttpOnly cookie))
                assertEqual "sameSite" CookieSameSiteLax (cookieSameSite cookie)
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "should properly report httpOnly cookie" $
        withApplication (pure httpOnlyCookie) $ \port ->
          withChromiumContext $ \ctx page -> do
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            jar <- cookies ctx
            assertEqual "count" 1 (length jar)
            case jar of
              [cookie] -> do
                assertEqual "name" "name" (cookieNameToText (cookieName cookie))
                assertEqual "httpOnly" True (cookieHttpOnlyToBool (cookieHttpOnly cookie))
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "should properly report Strict sameSite cookie" $
        withApplication (pure strictCookie) $ \port ->
          withChromiumContext $ \ctx page -> do
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            jar <- cookies ctx
            case jar of
              [cookie] ->
                assertEqual "sameSite" CookieSameSiteStrict (cookieSameSite cookie)
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "should get multiple cookies" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "document" "password=1234; username=John Doe"
            =<< evaluateText
              page
              "() => { document.cookie = 'username=John Doe'; document.cookie = 'password=1234'; return document.cookie.split('; ').sort().join('; '); }"
          jar <- cookies (pageContext page)
          assertEqual "names" ["password", "username"] (sort (fmap (cookieNameToText . cookieName) jar))
          assertEqual "values" ["1234", "John Doe"] (fmap (cookieValueToText . cookieValue) (sortOn (cookieNameToText . cookieName) jar))
    , testCase "should get cookies from multiple urls" $
        withAssets $ \_ page -> do
          addCookies
            (pageContext page)
            [ (cookieForUrl (urlFromText "https://foo.com") (CookieName "doggo") (CookieValue "woofs")) {cookieToSetSameSite = Just CookieSameSiteNone}
            , (cookieForUrl (urlFromText "https://bar.com") (CookieName "catto") (CookieValue "purrs")) {cookieToSetSameSite = Just CookieSameSiteLax}
            , (cookieForUrl (urlFromText "https://baz.com") (CookieName "birdo") (CookieValue "tweets")) {cookieToSetSameSite = Just CookieSameSiteLax}
            ]
          jar <- cookiesFor (pageContext page) [urlFromText "https://foo.com", urlFromText "https://baz.com"]
          assertEqual "names" ["birdo", "doggo"] (sort (fmap (cookieNameToText . cookieName) jar))
    , testCase "should return secure cookies based on HTTP(S) protocol" $
        withAssets $ \_ page -> do
          addCookies
            (pageContext page)
            [ (cookieForUrl (urlFromText "https://foo.com") (CookieName "doggo") (CookieValue "woofs")) {cookieToSetSecure = Just (CookieSecure True)}
            , (cookieForUrl (urlFromText "http://foo.com") (CookieName "catto") (CookieValue "purrs")) {cookieToSetSecure = Just (CookieSecure False)}
            ]
          jar <- cookiesFor (pageContext page) [urlFromText "https://foo.com"]
          assertEqual "names" ["catto", "doggo"] (sort (fmap (cookieNameToText . cookieName) jar))
    , testCase "should work with subdomain cookie" $
        withAssets $ \_ page -> do
          addCookies
            (pageContext page)
            [ (cookieForHost (CookieDomain ".foo.com") (CookiePath "/") (CookieName "doggo") (CookieValue "woofs"))
                { cookieToSetSecure = Just (CookieSecure True)
                , cookieToSetSameSite = Just CookieSameSiteLax
                }
            ]
          onFoo <- cookiesFor (pageContext page) [urlFromText "https://foo.com"]
          assertEqual "foo.com domain" [".foo.com"] (fmap (cookieDomainToText . cookieDomain) onFoo)
          onSub <- cookiesFor (pageContext page) [urlFromText "https://sub.foo.com"]
          assertEqual "sub.foo.com names" ["doggo"] (fmap (cookieNameToText . cookieName) onSub)
    , testCase "should get a non-session cookie" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "document" "username=John Doe"
            =<< evaluateText
              page
              "() => { const date = new Date('1/1/2038'); document.cookie = `username=John Doe;expires=${date.toUTCString()}`; return document.cookie; }"
          jar <- cookies (pageContext page)
          assertEqual "names" ["username"] (fmap (cookieNameToText . cookieName) jar)
          assertEqual "values" ["John Doe"] (fmap (cookieValueToText . cookieValue) jar)
          assertBool "expires is set" (fmap ((/= -1) . cookieExpiresToScientific . cookieExpires) jar == [True])
    , testCase "should return cookies with empty value" $ do
        requests <- newIORef []
        withChromiumContext $ \ctx page ->
          withLocalApp (emptyCookieApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            jar <- cookies ctx
            assertEqual "names" ["name"] (fmap (cookieNameToText . cookieName) jar)
            assertEqual "values" [""] (fmap (cookieValueToText . cookieValue) jar)
    , testCase "should allow adding cookies with >400 days expiration" $
        withAssets $ \_ page -> do
          addCookies
            (pageContext page)
            [ CookieToSet
                { cookieToSetName = CookieName "username"
                , cookieToSetValue = CookieValue "John Doe"
                , cookieToSetUrl = Nothing
                , cookieToSetDomain = Just (CookieDomain "127.0.0.1")
                , cookieToSetPath = Just (CookiePath "/")
                , cookieToSetExpires = Just (CookieExpires 2000000000)
                , cookieToSetHttpOnly = Nothing
                , cookieToSetSecure = Nothing
                , cookieToSetSameSite = Just CookieSameSiteLax
                }
            ]
          jar <- cookies (pageContext page)
          assertEqual "names" ["username"] (fmap (cookieNameToText . cookieName) jar)
    , testCase "should add cookies with an expiration" $
        withChromiumPage $ \page -> do
          expires <- evaluateNumber page "() => Math.floor(Date.now()/1000) + 3600"
          addCookies
            (pageContext page)
            [ (cookieForUrl (urlFromText "https://foo.com") (CookieName "doggo") (CookieValue "woofs"))
                { cookieToSetSameSite = Just CookieSameSiteNone
                , cookieToSetExpires = Just (CookieExpires (realToFrac expires))
                }
            ]
          jar <- cookiesFor (pageContext page) [urlFromText "https://foo.com"]
          case jar of
            [cookie] -> do
              assertEqual "name" "doggo" (cookieNameToText (cookieName cookie))
              assertEqual "value" "woofs" (cookieValueToText (cookieValue cookie))
              assertEqual "domain" "foo.com" (cookieDomainToText (cookieDomain cookie))
              assertEqual "path" "/" (cookiePathToText (cookiePath cookie))
              assertEqual "expires" expires (realToFrac (cookieExpiresToScientific (cookieExpires cookie)))
              assertEqual "httpOnly" False (cookieHttpOnlyToBool (cookieHttpOnly cookie))
              assertEqual "secure" True (cookieSecureToBool (cookieSecure cookie))
              assertEqual "sameSite" CookieSameSiteNone (cookieSameSite cookie)
            _ ->
              assertEqual "one cookie" 1 (length jar)
          addCookies
            (pageContext page)
            [ (cookieForUrl (urlFromText "https://foo.com") (CookieName "doggo") (CookieValue "woofs"))
                { cookieToSetSameSite = Just CookieSameSiteNone
                , cookieToSetExpires = Just (CookieExpires 253402300799)
                }
            ]
          assertProtocolContains "Cookie should have a valid expires" $
            addCookies
              (pageContext page)
              [ (cookieForUrl (urlFromText "https://foo.com") (CookieName "doggo") (CookieValue "woofs"))
                  { cookieToSetSameSite = Just CookieSameSiteNone
                  , cookieToSetExpires = Just (CookieExpires 253402300800)
                  }
              ]
          assertProtocolContains "Cookie should have a valid expires" $
            addCookies
              (pageContext page)
              [ (cookieForUrl (urlFromText "https://foo.com") (CookieName "doggo") (CookieValue "woofs"))
                  { cookieToSetSameSite = Just CookieSameSiteNone
                  , cookieToSetExpires = Just (CookieExpires (-42))
                  }
              ]
    , testCase "should parse cookie with large Max-Age correctly" $
        withChromiumContext $ \ctx page ->
          withLocalApp maxAgeApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/foobar")) defaultGotoOptions
            documentCookie <- evaluateText page "() => document.cookie"
            assertEqual "document" "cookie1=value1" documentCookie
            jar <- cookies ctx
            assertEqual "count" 1 (length jar)
            case jar of
              [cookie] -> do
                assertEqual "name" "cookie1" (cookieNameToText (cookieName cookie))
                assertEqual "domain" "127.0.0.1" (cookieDomainToText (cookieDomain cookie))
                assertEqual "path" "/" (cookiePathToText (cookiePath cookie))
                assertBool "expires is set" ((/= -1) (cookieExpiresToScientific (cookieExpires cookie)))
                assertEqual "httpOnly" False (cookieHttpOnlyToBool (cookieHttpOnly cookie))
                assertEqual "secure" False (cookieSecureToBool (cookieSecure cookie))
                assertEqual "sameSite" CookieSameSiteLax (cookieSameSite cookie)
              _ ->
                assertEqual "one cookie" 1 (length jar)
    , testCase "iframe should inherit cookies from parent" $
        withChromiumPage $ \page -> do
          route page (UrlMatchGlob "http://sub.example.test/**") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillBody = Just (FulfillText iframeSubHtml)
                , fulfillContentType = Just (ContentType "text/html")
                }
          route page (UrlMatchGlob "http://example.test/**") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillBody = Just (FulfillText iframeMainHtml)
                , fulfillContentType = Just (ContentType "text/html")
                , fulfillHeaders =
                    [ HttpHeader
                        (HeaderName "set-cookie")
                        (HeaderValue "testCookie=value; SameSite=Lax; Domain=example.test")
                    ]
                }
          _ <- goto page (urlFromText "http://example.test/") defaultGotoOptions
          mainBody <- textContent (locate page "body")
          assertBool "main body has cookie" (maybe False ("testCookie=value" `isInfixOf`) mainBody)
          frameBody <-
            textContent
              (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "body"))
          assertBool "iframe body has cookie" (maybe False ("testCookie=value" `isInfixOf`) frameBody)
    -- Upstream skips "should support requestStorageAccess" when the browser is
    -- Chromium, and this harness only runs Chromium, so there is no case to port.
    ]

iframeMainHtml :: Text
iframeMainHtml =
  "<p id=result></p><script>document.getElementById('result').textContent = document.cookie;</script><iframe src=\"http://sub.example.test/\"></iframe>"

iframeSubHtml :: Text
iframeSubHtml =
  "<p id=result></p><script>document.getElementById('result').textContent = document.cookie || 'no cookies';</script>"

emptyHtml :: Application
emptyHtml _req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/html")]
        (LBS8.pack "<html></html>")
    )

httpOnlyCookie :: Application
httpOnlyCookie _req respond =
  respond
    ( responseLBS
        status200
        [ (hContentType, BS8.pack "text/html")
        , ("Set-Cookie", BS8.pack "name=value;HttpOnly; Path=/")
        ]
        (LBS8.pack "<html></html>")
    )

strictCookie :: Application
strictCookie _req respond =
  respond
    ( responseLBS
        status200
        [ (hContentType, BS8.pack "text/html")
        , ("Set-Cookie", BS8.pack "name=value;SameSite=Strict")
        ]
        (LBS8.pack "<html></html>")
    )

-- | Records every request. Sets an empty-valued cookie on /empty.html and
-- answers every path with an empty page.
emptyCookieApp :: IORef [Recorded] -> Application
emptyCookieApp requests request respond = do
  recordRequest requests request
  case pathInfo request of
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html"), ("Set-Cookie", "name=;Path=/")] "")
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")

-- | Sets a cookie with a Max-Age far larger than any browser will store.
maxAgeApp :: Application
maxAgeApp _req respond =
  respond
    ( responseLBS
        status200
        [ (hContentType, BS8.pack "text/html")
        , ("Set-Cookie", BS8.pack "cookie1=value1; Path=/; Expires=Thu, 08 Sep 2270 15:06:12 GMT; Max-Age=7776000000")
        ]
        (LBS8.pack "<html></html>")
    )

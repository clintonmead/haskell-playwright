module Playwright.BrowserContextProxySpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List
  ( sort
  )
import Data.Text
  ( Text
  , pack
  , unpack
  )
import Network.HTTP.Types
  ( Status
  , hContentType
  , mkStatus
  , status200
  )
import Network.Wai
  ( Application
  , Request
  , Response
  , pathInfo
  , rawPathInfo
  , requestHeaders
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Browser
  , Cookie (cookieDomain, cookieName, cookieValue)
  , CookieDomain (cookieDomainToText)
  , CookieName (cookieNameToText)
  , CookieValue (cookieValueToText)
  , HttpPassword (HttpPassword)
  , HttpUsername (HttpUsername)
  , NewContextOptions (newContextProxy)
  , Page
  , Playwright
  , ProxyHost (ProxyHost)
  , ProxySettings (ProxySettings, proxySettingsBypass, proxySettingsPassword, proxySettingsServer, proxySettingsUsername)
  , closePage
  , contextRequest
  , cookies
  , cookiesFor
  , defaultGotoOptions
  , defaultNewContextOptions
  , fetchGet
  , getByTestId
  , goto
  , innerText
  , newContextWith
  , newPage
  , title
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumBrowser
  , withChromiumContextWith
  , assertEqual
  )
import Playwright.ProxySocksSupport
  ( socksProxyUrl
  , withSocksMockServer
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
    "browsercontext-proxy"
    [ testCase "should work when passing the proxy only on the context level" $
        withProxyApp targetApp $ \port ->
          withContextProxy (httpProxy port Nothing Nothing Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should use proxy" $
        withProxyApp targetApp $ \port ->
          withContextProxy (httpProxy port Nothing Nothing Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should use proxy twice" $
        withProxyApp targetApp $ \port ->
          withContextProxy (httpProxy port Nothing Nothing Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            _ <- goto page (urlFromText "http://non-existent-2.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should use proxy for second page" $
        withProxyApp targetApp $ \port ->
          withChromiumContextWith (proxyOpts (httpProxy port Nothing Nothing Nothing)) $ \ctx page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "first" "Served by the proxy" =<< title page
            page2 <- newPage ctx
            _ <- goto page2 (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "second" "Served by the proxy" =<< title page2
    , testCase "should work with IP:PORT notion" $
        withProxyApp targetApp $ \port ->
          withContextProxy (ipProxy port) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should use socks proxy in second page" $
        withSocksMockServer $ \port ->
          withChromiumContextWith (proxyOpts (httpProxyHost (socksProxyUrl port))) $ \ctx page -> do
            _ <- goto page (urlFromText "http://non-existent.com") defaultGotoOptions
            assertEqual "first" "Served by the SOCKS proxy" =<< title page
            page2 <- newPage ctx
            _ <- goto page2 (urlFromText "http://non-existent.com") defaultGotoOptions
            assertEqual "second" "Served by the SOCKS proxy" =<< title page2
    , testCase "should throw for socks5 authentication" $
        withChromiumBrowser $ \browser ->
          assertSocksAuth
            browser
            "socks5://localhost:1234"
            "Browser does not support socks5 proxy authentication"
    , testCase "should throw for socks4 authentication" $
        withChromiumBrowser $ \browser ->
          assertSocksAuth
            browser
            "socks4://localhost:1234"
            "Socks4 proxy protocol does not support authentication"
    , testCase "should authenticate" $
        withProxyApp authApp $ \port ->
          withContextProxy (httpProxy port (Just (HttpUsername "user")) (Just (HttpPassword "secret")) Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should authenticate with empty password" $
        withProxyApp authApp $ \port ->
          withContextProxy (httpProxy port (Just (HttpUsername "user")) (Just (HttpPassword "")) Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should isolate proxy credentials between contexts" $
        withProxyApp authApp $ \port -> do
          withContextProxy (httpProxy port (Just (HttpUsername "user1")) (Just (HttpPassword "secret1")) Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "user1" "Served by the proxy" =<< title page
          withContextProxy (httpProxy port (Just (HttpUsername "user2")) (Just (HttpPassword "secret2")) Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "user2" "Served by the proxy" =<< title page
    , testCase "should exclude patterns" $
        excludePatternsTest
    , testCase "does launch without a port" $
        withChromiumContextWith (proxyOpts (httpProxyHost "http://localhost")) $ \_ctx _page ->
          pure ()
    , testCase "should isolate proxy credentials between contexts on navigation" $
        isolateOnNavigation
    , testCase "should send secure cookies to subdomain.localhost" $
        sendSecureCookiesToSubdomain
    , testCase "should set cookie for top-level domain" $
        setCookieForTopLevelDomain
    , testGroup
        "should proxy local network requests"
        [ testGroup
            label
            [ testCase (unpack description) $
                localNetworkProxyTest additionalBypass target
            | (target, description) <- localTargets
            ]
        | (additionalBypass, label) <- [(False, "by default"), (True, "with other bypasses")]
        ]
    ]

-- Upstream cases not ported here:
--   should throw for bad server value: ProxyHost is a Text newtype, so a non-string server cannot be built.
--   should use proxy for https urls: the harness has no TLS fixture and Warp cannot serve a CONNECT tunnel.
--   should use ipv6 proxy: a Warp app on ::1 with an [::1]:port proxy failed with
--     net::ERR_PROXY_CONNECTION_FAILED. The root cause was not confirmed, though the sandbox has ::1 on lo.

localTargets :: [(Text, Text)]
localTargets =
  [ ("localhost", "localhost")
  , ("127.0.0.1", "loopback address")
  , ("169.254.3.4", "link-local")
  ]

localNetworkProxyTest :: Bool -> Text -> IO ()
localNetworkProxyTest additionalBypass target =
  withProxyApp anyTargetApp $ \port ->
    withChromiumContextWith (proxyOpts (httpProxy port Nothing Nothing bypass)) $ \_ctx page -> do
      _ <- goto page (urlFromText ("http://" <> target <> ":55555/target.html")) defaultGotoOptions
      assertEqual "title" "Served by the proxy" =<< title page
      if additionalBypass
        then assertProtocolContains "ERR_" (goto page (urlFromText otherHost) defaultGotoOptions)
        else do
          _ <- goto page (urlFromText otherHost) defaultGotoOptions
          pure ()
  where
    bypass :: Maybe Text
    bypass = if additionalBypass then Just "1.non.existent.domain.for.the.test" else Nothing
    otherHost :: Text
    otherHost = "http://1.non.existent.domain.for.the.test/foo.html"

sendSecureCookiesToSubdomain :: IO ()
sendSecureCookiesToSubdomain =
  withProxyApp cookieApp $ \port ->
    withChromiumContextWith (proxyOpts (httpProxy port Nothing Nothing Nothing)) $ \ctx page -> do
      _ <- goto page (urlFromText "http://subdomain.localhost/set-cookie.html") defaultGotoOptions
      stored <- cookiesFor ctx [urlFromText "http://subdomain.localhost"]
      assertEqual
        "cookies"
        [("non-secure", "subdomain.localhost"), ("secure", "subdomain.localhost")]
        (map cookieNameAndDomain stored)
      _ <- goto page (urlFromText "http://subdomain.localhost/read-cookie.html") defaultGotoOptions
      assertEqual "cookie header" "Cookie: non-secure=1; secure=1" =<< innerText (getByTestId page "cookie")

setCookieForTopLevelDomain :: IO ()
setCookieForTopLevelDomain =
  withProxyApp cookieApp $ \port ->
    withChromiumContextWith (proxyOpts (httpProxy port Nothing Nothing Nothing)) $ \ctx _page -> do
      apiCtx <- contextRequest ctx
      _ <- fetchGet apiCtx (urlFromText "http://codes/empty.html")
      found <- cookies ctx
      assertEqual "cookies" [("name", "val")] (map cookieNameAndValue found)

cookieNameAndDomain :: Cookie -> (Text, Text)
cookieNameAndDomain c =
  (cookieNameToText (cookieName c), cookieDomainToText (cookieDomain c))

cookieNameAndValue :: Cookie -> (Text, Text)
cookieNameAndValue c =
  (cookieNameToText (cookieName c), cookieValueToText (cookieValue c))

cookieApp :: Application
cookieApp request respond
  | wantsPath request "set-cookie.html" =
      respond
        ( responseLBS
            status200
            [ ("Set-Cookie", BS8.pack "non-secure=1; HttpOnly")
            , ("Set-Cookie", BS8.pack "secure=1; HttpOnly; Secure")
            ]
            ""
        )
  | wantsPath request "read-cookie.html" =
      respond (htmlBody ("<div data-testid=cookie>Cookie: " <> sortedCookieHeader request <> "</div>"))
  | wantsPath request "empty.html" =
      respond (responseLBS status200 [("Set-Cookie", BS8.pack "name=val; Domain=codes; Path=/;")] "")
  | otherwise =
      respond (responseLBS status200 [] "")

sortedCookieHeader :: Request -> String
sortedCookieHeader request =
  BS8.unpack (BS8.intercalate (BS8.pack "; ") (sort (map BS8.strip (BS8.split ';' header))))
  where
    header = case lookup "cookie" (requestHeaders request) of
      Just value -> value
      Nothing -> BS8.empty

anyTargetApp :: Application
anyTargetApp _request respond =
  respond (htmlTitle "Served by the proxy")

isolateOnNavigation :: IO ()
isolateOnNavigation =
  withProxyApp navigationAuthApp $ \port ->
    withChromiumBrowser $ \browser -> do
      ctx1 <-
        newContextWith
          browser
          (proxyOpts (httpProxy port (Just (HttpUsername "user1")) (Just (HttpPassword "secret1")) Nothing))
      page1 <- newPage ctx1
      _ <- goto page1 (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
      assertEqual "user1" "user1" =<< innerText (getByTestId page1 "user")
      ctx2 <-
        newContextWith
          browser
          (proxyOpts (httpProxy port (Just (HttpUsername "user2")) (Just (HttpPassword "secret2")) Nothing))
      page2 <- newPage ctx2
      _ <- goto page2 (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
      assertEqual "user2" "user2" =<< innerText (getByTestId page2 "user")
      _ <- goto page1 (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
      assertEqual "user1 again" "user1" =<< innerText (getByTestId page1 "user")

assertSocksAuth :: Browser -> Text -> Text -> Playwright ()
assertSocksAuth browser server needle =
  assertProtocolContains needle $ do
    _ <-
      newContextWith
        browser
        ( proxyOpts
            (httpProxyHost server)
              { proxySettingsUsername = Just (HttpUsername "user")
              , proxySettingsPassword = Just (HttpPassword "secret")
              }
        )
    pure ()

excludePatternsTest :: IO ()
excludePatternsTest =
  withProxyApp targetApp $ \port ->
    withChromiumContextWith
      ( proxyOpts
          (httpProxy port Nothing Nothing (Just "1.non.existent.domain.for.the.test, 2.non.existent.domain.for.the.test, .another.test"))
      )
      $ \ctx page0 -> do
        _ <- goto page0 (urlFromText "http://0.non.existent.domain.for.the.test/target.html") defaultGotoOptions
        assertEqual "proxied" "Served by the proxy" =<< title page0
        closePage page0
        page1 <- newPage ctx
        assertProtocolContains
          "ERR_"
          (goto page1 (urlFromText "http://1.non.existent.domain.for.the.test/target.html") defaultGotoOptions)
        closePage page1
        page2 <- newPage ctx
        assertProtocolContains
          "ERR_"
          (goto page2 (urlFromText "http://2.non.existent.domain.for.the.test/target.html") defaultGotoOptions)
        closePage page2
        page3 <- newPage ctx
        assertProtocolContains
          "ERR_"
          (goto page3 (urlFromText "http://foo.is.the.another.test/target.html") defaultGotoOptions)
        closePage page3
        page4 <- newPage ctx
        _ <- goto page4 (urlFromText "http://3.non.existent.domain.for.the.test/target.html") defaultGotoOptions
        assertEqual "still proxied" "Served by the proxy" =<< title page4

withContextProxy :: ProxySettings -> (Page -> Playwright a) -> IO a
withContextProxy settings action =
  withChromiumContextWith (proxyOpts settings) $ \_ page ->
    action page

proxyOpts :: ProxySettings -> NewContextOptions
proxyOpts settings =
  defaultNewContextOptions {newContextProxy = Just settings}

httpProxy :: Int -> Maybe HttpUsername -> Maybe HttpPassword -> Maybe Text -> ProxySettings
httpProxy port username password bypass =
  (httpProxyHost ("localhost:" <> pack (show port)))
    { proxySettingsUsername = username
    , proxySettingsPassword = password
    , proxySettingsBypass = bypass
    }

ipProxy :: Int -> ProxySettings
ipProxy port =
  httpProxyHost ("127.0.0.1:" <> pack (show port))

httpProxyHost :: Text -> ProxySettings
httpProxyHost host =
  ProxySettings
    { proxySettingsServer = ProxyHost host
    , proxySettingsBypass = Nothing
    , proxySettingsUsername = Nothing
    , proxySettingsPassword = Nothing
    }

withProxyApp :: Application -> (Int -> IO a) -> IO a
withProxyApp =
  withApplication . pure

targetApp :: Application
targetApp request respond =
  if wantsPath request "target.html"
    then respond (htmlTitle "Served by the proxy")
    else respond (responseLBS status200 [] "")

authApp :: Application
authApp request respond =
  case lookup "proxy-authorization" (requestHeaders request) of
    Nothing ->
      respond proxyAuthRequired
    Just _ ->
      respond (htmlTitle "Served by the proxy")

navigationAuthApp :: Application
navigationAuthApp request respond =
  case lookup "proxy-authorization" (requestHeaders request) of
    Nothing ->
      respond proxyAuthRequired
    Just auth ->
      let
        user = usernameFromBasic auth
      in
        respond
          ( htmlBody
              ("Hello <div data-testid=user>" <> user <> "</div>!\n")
          )

usernameFromBasic :: BS8.ByteString -> String
usernameFromBasic header =
  case BS8.split ' ' header of
    [_, encoded] ->
      case BS8.split ':' (decodeBasic encoded) of
        user : _ ->
          BS8.unpack user
        [] ->
          ""
    _ ->
      ""

decodeBasic :: BS8.ByteString -> BS8.ByteString
decodeBasic encoded =
  case encoded of
    "dXNlcjE6c2VjcmV0MQ==" ->
      BS8.pack "user1:secret1"
    "dXNlcjI6c2VjcmV0Mg==" ->
      BS8.pack "user2:secret2"
    "dXNlcjpzZWNyZXQ=" ->
      BS8.pack "user:secret"
    "dXNlcjo=" ->
      BS8.pack "user:"
    _ ->
      encoded

proxyAuthRequired :: Response
proxyAuthRequired =
  responseLBS
    status407
    [("Proxy-Authenticate", BS8.pack "Basic realm=\"proxy\"")]
    (LBS8.pack "Proxy authorization required")

wantsPath :: Request -> Text -> Bool
wantsPath request name =
  pathInfo request == [name]
    || BS8.pack (unpack name) `BS8.isInfixOf` rawPathInfo request

htmlTitle :: Text -> Response
htmlTitle titleText =
  htmlBody ("<html><title>" <> unpack titleText <> "</title></html>")

htmlBody :: String -> Response
htmlBody body =
  responseLBS
    status200
    [(hContentType, BS8.pack "text/html")]
    (LBS8.pack body)

status407 :: Status
status407 =
  mkStatus 407 "Proxy Authentication Required"

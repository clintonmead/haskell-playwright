module Playwright.ProxySpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( Text
  , pack
  , unpack
  )
import Network.HTTP.Types
  ( Status
  , hContentType
  , hLocation
  , mkStatus
  , status200
  , status302
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
  ( HttpPassword (HttpPassword)
  , HttpUsername (HttpUsername)
  , LaunchOptions (launchOptionsProxy)
  , Page
  , Playwright
  , ProxyHost (ProxyHost)
  , ProxySettings (ProxySettings, proxySettingsBypass, proxySettingsPassword, proxySettingsServer, proxySettingsUsername)
  , closePage
  , defaultGotoOptions
  , defaultLaunchOptions
  , goto
  , newBrowserPage
  , title
  , urlFromText
  , withPage
  )
import Playwright.Harness
  ( assertProtocolContains
  , withChromiumBrowserWith
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

-- Upstream cases not ported here:
--   should throw for bad server value: ProxyHost is a Text newtype, so a non-string server cannot be built.
--   should use proxy with emulated user agent: upstream marks it fixme(true); the non-emulated
--     user agent is still sent in the CONNECT request.
--   should reconnect with credentials after CONNECT 407 closes the socket: needs an HTTPS fixture,
--     a CONNECT tunnel and a proxy auth handler, and Warp cannot serve CONNECT.
--   should use SOCKS proxy for websocket requests, should use http proxy for websocket requests:
--     they resolve fake hosts through public DNS (nip.io) and inspect CONNECT and websocket
--     traffic at the proxy, which the Warp-based test proxy cannot record.

tests :: TestTree
tests =
  testGroup
    "proxy"
    [ testCase "should use proxy" $
        withProxyApp targetApp $ \port ->
          withLaunchProxy (httpProxy port Nothing Nothing Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should use proxy for second page" $
        withProxyApp targetApp $ \port ->
          withChromiumBrowserWith (launchWithProxy (httpProxy port Nothing Nothing Nothing)) $ \browser -> do
            page <- newBrowserPage browser
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "first" "Served by the proxy" =<< title page
            page2 <- newBrowserPage browser
            _ <- goto page2 (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "second" "Served by the proxy" =<< title page2
    , testCase "should work with IP:PORT notion" $
        withProxyApp targetApp $ \port ->
          withLaunchProxy (ipProxy port) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should authenticate" $
        withProxyApp authApp $ \port ->
          withLaunchProxy (httpProxy port (Just (HttpUsername "user")) (Just (HttpPassword "secret")) Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/target.html") defaultGotoOptions
            assertEqual "auth" "Basic dXNlcjpzZWNyZXQ=" =<< title page
    , testCase "should work with authenticate followed by redirect" $
        withProxyApp redirectAuthApp $ \port ->
          withLaunchProxy (httpProxy port (Just (HttpUsername "user")) (Just (HttpPassword "secret")) Nothing) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com/page1.html") defaultGotoOptions
            assertEqual "title" "Served by the proxy" =<< title page
    , testCase "should exclude patterns" $
        withProxyApp targetApp $ \port ->
          withChromiumBrowserWith
            ( launchWithProxy
                (httpProxy port Nothing Nothing (Just "1.non.existent.domain.for.the.test, 2.non.existent.domain.for.the.test, .another.test"))
            )
            $ \browser -> do
              page0 <- newBrowserPage browser
              _ <- goto page0 (urlFromText "http://0.non.existent.domain.for.the.test/target.html") defaultGotoOptions
              assertEqual "proxied" "Served by the proxy" =<< title page0
              closePage page0
              page1 <- newBrowserPage browser
              assertProtocolContains
                "ERR_"
                (goto page1 (urlFromText "http://1.non.existent.domain.for.the.test/target.html") defaultGotoOptions)
              closePage page1
              page2 <- newBrowserPage browser
              assertProtocolContains
                "ERR_"
                (goto page2 (urlFromText "http://2.non.existent.domain.for.the.test/target.html") defaultGotoOptions)
              closePage page2
              page3 <- newBrowserPage browser
              assertProtocolContains
                "ERR_"
                (goto page3 (urlFromText "http://foo.is.the.another.test/target.html") defaultGotoOptions)
              closePage page3
              page4 <- newBrowserPage browser
              _ <- goto page4 (urlFromText "http://3.non.existent.domain.for.the.test/target.html") defaultGotoOptions
              assertEqual "still proxied" "Served by the proxy" =<< title page4
    , testCase "should use socks proxy" $
        withSocksMockServer $ \port ->
          withLaunchProxy (httpProxyHost (socksProxyUrl port)) $ \page -> do
            _ <- goto page (urlFromText "http://non-existent.com") defaultGotoOptions
            assertEqual "title" "Served by the SOCKS proxy" =<< title page
    , testCase "should use socks proxy in second page" $
        withSocksMockServer $ \port ->
          withChromiumBrowserWith (launchWithProxy (httpProxyHost (socksProxyUrl port))) $ \browser -> do
            page <- newBrowserPage browser
            _ <- goto page (urlFromText "http://non-existent.com") defaultGotoOptions
            assertEqual "first" "Served by the SOCKS proxy" =<< title page
            page2 <- newBrowserPage browser
            _ <- goto page2 (urlFromText "http://non-existent.com") defaultGotoOptions
            assertEqual "second" "Served by the SOCKS proxy" =<< title page2
    , testCase "does launch without a port" $
        withChromiumBrowserWith
          (launchWithProxy (httpProxyHost "http://localhost"))
          $ \_browser ->
            pure ()
    , testGroup
        "should proxy local network requests"
        [ testGroup
            "by default"
            [ testCase "localhost" $ localNetworkCase Nothing "localhost"
            , testCase "loopback address" $ localNetworkCase Nothing "127.0.0.1"
            , testCase "link-local" $ localNetworkCase Nothing "169.254.169.254"
            ]
        , testGroup
            "with other bypasses"
            [ testCase "localhost" $ localNetworkCase (Just "example.com") "localhost"
            , testCase "loopback address" $ localNetworkCase (Just "example.com") "127.0.0.1"
            , testCase "link-local" $ localNetworkCase (Just "example.com") "169.254.169.254"
            ]
        ]
    , testGroup
        "should allow bypassing localhost requests"
        [ testCase "localhost" $ bypassCase "localhost" "localhost"
        , testCase "127.0.0.1" $ bypassCase "127.0.0.1" "127.0.0.1"
        ]
    , testGroup
        "should bypass proxy for localhost when localhost is in bypass list"
        [ testCase "localhost" $ bypassCase "localhost, 127.0.0.1" "localhost"
        , testCase "127.0.0.1" $ bypassCase "localhost, 127.0.0.1" "127.0.0.1"
        ]
    ]

-- Requests to local hosts are sent through the proxy, which records them and
-- answers with "Served by the proxy".
localNetworkCase :: Maybe Text -> Text -> IO ()
localNetworkCase bypass host = do
  ref <- newIORef []
  withApplication (pure (recordingProxy ref)) $ \proxyPort ->
    withApplication (pure (titleApp "Served by the server")) $ \serverPort ->
      withLaunchProxy (httpProxy proxyPort Nothing Nothing bypass) $ \page -> do
        _ <- goto page (urlFromText ("http://" <> host <> ":" <> pack (show serverPort) <> "/target.html")) defaultGotoOptions
        assertEqual "title" "Served by the proxy" =<< title page
  seen <- readIORef ref
  assertEqual "proxy recorded the request" True (not (null seen))

-- A host listed in the bypass list is reached directly, not through the proxy.
bypassCase :: Text -> Text -> IO ()
bypassCase bypassList host =
  withApplication (pure (titleApp "Served by the proxy")) $ \proxyPort ->
    withApplication (pure (titleApp "Served by the server")) $ \serverPort ->
      withLaunchProxy (httpProxy proxyPort Nothing Nothing (Just bypassList)) $ \page -> do
        _ <- goto page (urlFromText ("http://" <> host <> ":" <> pack (show serverPort) <> "/proxied/target.html")) defaultGotoOptions
        assertEqual "title" "Served by the server" =<< title page

recordingProxy :: IORef [Text] -> Application
recordingProxy ref request respond = do
  let
    host = maybe "" (pack . BS8.unpack) (lookup "host" (requestHeaders request))
    path = pack (BS8.unpack (rawPathInfo request))
  modifyIORef' ref (<> [host <> path])
  respond (html "Served by the proxy")

titleApp :: Text -> Application
titleApp titleText _ respond =
  respond (html titleText)

withLaunchProxy :: ProxySettings -> (Page -> Playwright a) -> IO a
withLaunchProxy settings action =
  withChromiumBrowserWith (launchWithProxy settings) $ \browser ->
    withPage browser action

launchWithProxy :: ProxySettings -> LaunchOptions
launchWithProxy settings =
  defaultLaunchOptions {launchOptionsProxy = Just settings}

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
    then respond (html "Served by the proxy")
    else respond (responseLBS status200 [] "")

authApp :: Application
authApp request respond =
  case lookup "proxy-authorization" (requestHeaders request) of
    Nothing ->
      respond
        ( responseLBS
            status407
            [("Proxy-Authenticate", BS8.pack "Basic realm=\"Access to internal site\"")]
            LBS8.empty
        )
    Just auth ->
      respond (html (pack (BS8.unpack auth)))

redirectAuthApp :: Application
redirectAuthApp request respond =
  case lookup "proxy-authorization" (requestHeaders request) of
    Nothing ->
      respond
        ( responseLBS
            status407
            [("Proxy-Authenticate", BS8.pack "Basic realm=\"Access to internal site\"")]
            LBS8.empty
        )
    Just _
      | wantsPath request "page1.html" ->
          respond (responseLBS status302 [(hLocation, BS8.pack "/page2.html")] LBS8.empty)
      | wantsPath request "page2.html" ->
          respond (html "Served by the proxy")
      | otherwise ->
          respond (html "Served by the proxy")

wantsPath :: Request -> Text -> Bool
wantsPath request name =
  pathInfo request == [name]
    || BS8.pack (unpack name) `BS8.isInfixOf` rawPathInfo request

html :: Text -> Response
html titleText =
  responseLBS
    status200
    [(hContentType, BS8.pack "text/html")]
    (LBS8.pack ("<html><title>" <> unpack titleText <> "</title></html>"))

status407 :: Status
status407 =
  mkStatus 407 "Proxy Authentication Required"

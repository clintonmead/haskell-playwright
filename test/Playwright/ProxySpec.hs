module Playwright.ProxySpec
  ( tests
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
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
    , testCase "does launch without a port" $
        withChromiumBrowserWith
          (launchWithProxy (httpProxyHost "http://localhost"))
          $ \_browser ->
            pure ()
    ]

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

module Playwright.FetchProxySpec
  ( tests
  ) where

import Control.Exception
  ( bracket
  )
import Control.Monad
  ( unless
  )
import Control.Monad.IO.Class
  ( liftIO
  )
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
  , isInfixOf
  , pack
  , unpack
  )
import Network.HTTP.Types
  ( Status
  , hContentType
  , mkStatus
  , status200
  , status400
  )
import Network.Socket
  ( Family (AF_INET)
  , SockAddr (SockAddrInet)
  , Socket
  , SocketType (Stream)
  , close
  , connect
  , defaultProtocol
  , socket
  , tupleToHostAddress
  )
import Network.Socket.ByteString qualified as NSB
import Network.Wai
  ( Application
  , Response
  , rawPathInfo
  , requestHeaders
  , requestMethod
  , responseLBS
  , responseRaw
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( HttpPassword (HttpPassword)
  , HttpUsername (HttpUsername)
  , LaunchOptions (launchOptionsProxy)
  , NewContextOptions (newContextProxy)
  , NewRequestOptions (newRequestProxy)
  , Playwright
  , ProxyHost (ProxyHost)
  , ProxySettings (ProxySettings, proxySettingsBypass, proxySettingsPassword, proxySettingsServer, proxySettingsUsername)
  , apiResponseStatus
  , apiResponseText
  , contextRequest
  , defaultLaunchOptions
  , defaultNewContextOptions
  , defaultNewRequestOptions
  , disposeAPIRequestContext
  , fetchGet
  , httpStatusToInt
  , newRequestContextWith
  , runPlaywright
  , urlFromText
  )
import Playwright.BrowserContextFetchSupport
  ( assertNetworkError
  )
import Playwright.Harness
  ( assertEqual
  , withChromiumContextLaunchWith
  , withChromiumContextWith
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( race_
  )

-- Official cases that stay skipped:
--
-- * "should use socks proxy": needs a SOCKS5 fixture on a port the
--   test can read (socksPort); the Haskell harness has no SOCKS server.
-- * "should send correct ALPN protocol to HTTPS proxy": needs an
--   HTTPS proxy with a Node ALPNCallback and the upstream TLS cert
--   helper (TestServer.certOptions); the harness has no TLS proxy.
--
-- The proxy is a Warp app that answers CONNECT with a raw tunnel to a
-- local target app, which replaces upstream's forwardTo(server.PORT).
tests :: TestTree
tests =
  testGroup
    "FetchProxy"
    [ testCase "context request should pick up proxy credentials" contextCredentialsTest
    , testCase "global request should pick up proxy credentials" globalCredentialsTest
    , testCase "should work with context level proxy" contextLevelProxyTest
    , testCase "should support proxy.bypass" bypassTest
    ]

-- | Browser launch proxy with credentials; the context request must send
-- Proxy-Authorization for the user:secret pair.
contextCredentialsTest :: IO ()
contextCredentialsTest =
  withFixture True $ \fixture ->
    withChromiumContextLaunchWith
      defaultLaunchOptions {launchOptionsProxy = Just (credentialedProxy (fixturePort fixture))}
      defaultNewContextOptions
      $ \ctx _page -> do
        request <- contextRequest ctx
        response <- fetchGet request (urlFromText "http://non-existent.com/simple.json")
        assertProxyAuth fixture
        assertEqual "CONNECT" True =<< connectSeen fixture "non-existent.com:80"
        assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
        assertEqual "body" simpleText =<< apiResponseText response

-- | Global APIRequest context with a credentialed proxy in its options.
globalCredentialsTest :: IO ()
globalCredentialsTest =
  withFixture True $ \fixture ->
    runPlaywright $ do
      request <- newRequestContextWith defaultNewRequestOptions {newRequestProxy = Just (credentialedProxy (fixturePort fixture))}
      response <- fetchGet request (urlFromText "http://non-existent.com/simple.json")
      assertProxyAuth fixture
      assertEqual "CONNECT" True =<< connectSeen fixture "non-existent.com:80"
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "body" simpleText =<< apiResponseText response
      disposeAPIRequestContext request

-- | Context-level proxy without credentials; the target must be reached via the proxy.
contextLevelProxyTest :: IO ()
contextLevelProxyTest =
  withFixture False $ \fixture ->
    withChromiumContextWith defaultNewContextOptions {newContextProxy = Just (httpProxyHost ("localhost:" <> pack (show (fixturePort fixture))))} $ \ctx _page -> do
      request <- contextRequest ctx
      response <- fetchGet request (urlFromText "http://non-existent.com/target.html")
      assertEqual "status" 200 (httpStatusToInt (apiResponseStatus response))
      assertEqual "CONNECT" True =<< connectSeen fixture "non-existent.com:80"
      paths <- liftIO (readIORef (fixtureTargetPaths fixture))
      assertEqual "target reached" True (any ("/target.html" `isInfixOf`) paths)

-- | Bypassed hosts must go direct (and fail to resolve), other hosts via the proxy.
bypassTest :: IO ()
bypassTest =
  withFixture False $ \fixture ->
    withChromiumContextWith defaultNewContextOptions {newContextProxy = Just (bypassProxy (fixturePort fixture))} $ \ctx _page -> do
      request <- contextRequest ctx
      proxied0 <- fetchGet request (urlFromText "http://0.non.existent.domain.for.the.test/target.html")
      assertEqual "proxied 0 status" 200 (httpStatusToInt (apiResponseStatus proxied0))
      body0 <- apiResponseText proxied0
      assertEqual "proxied 0 body" True ("Served by the proxy" `isInfixOf` body0)
      assertNetworkError (fetchGet request (urlFromText "http://1.non.existent.domain.for.the.test/target.html"))
      assertNetworkError (fetchGet request (urlFromText "http://2.non.existent.domain.for.the.test/target.html"))
      assertNetworkError (fetchGet request (urlFromText "http://foo.is.the.another.test/target.html"))
      proxied3 <- fetchGet request (urlFromText "http://3.non.existent.domain.for.the.test/target.html")
      assertEqual "proxied 3 status" 200 (httpStatusToInt (apiResponseStatus proxied3))
      connects <- liftIO (readIORef (fixtureConnects fixture))
      assertEqual "proxied host tunnelled" True (any ("0.non.existent" `isInfixOf`) connects)
      assertEqual "bypassed hosts never tunnelled" False (any bypassedHost connects)
  where
    bypassedHost authority =
      any (`isInfixOf` authority) ["1.non.existent", "2.non.existent", "another.test"]

-- | Fixture: a target Warp app, and a Warp proxy app that tunnels CONNECT to it.
data Fixture = Fixture
  { fixturePort :: Int
  , fixtureConnects :: IORef [Text]
  , fixtureProxyAuths :: IORef [Maybe BS8.ByteString]
  , fixtureTargetPaths :: IORef [Text]
  }

withFixture :: Bool -> (Fixture -> IO a) -> IO a
withFixture requireAuth action = do
  connects <- newIORef []
  auths <- newIORef []
  paths <- newIORef []
  withApplication (pure (targetApp paths)) $ \targetPort ->
    withApplication (pure (proxyApp requireAuth targetPort connects auths)) $ \proxyPort ->
      action (Fixture proxyPort connects auths paths)

assertProxyAuth :: Fixture -> Playwright ()
assertProxyAuth fixture = do
  auths <- liftIO (readIORef (fixtureProxyAuths fixture))
  assertEqual "proxy auth" True (expectedAuth `elem` auths)

connectSeen :: Fixture -> Text -> Playwright Bool
connectSeen fixture authority = do
  connects <- liftIO (readIORef (fixtureConnects fixture))
  pure (authority `elem` connects)

bypassList :: Text
bypassList = "1.non.existent.domain.for.the.test, 2.non.existent.domain.for.the.test, .another.test"

bypassProxy :: Int -> ProxySettings
bypassProxy port =
  (httpProxyHost ("localhost:" <> pack (show port)))
    { proxySettingsBypass = Just bypassList
    }

credentialedProxy :: Int -> ProxySettings
credentialedProxy port =
  (httpProxyHost ("localhost:" <> pack (show port)))
    { proxySettingsUsername = Just (HttpUsername "user")
    , proxySettingsPassword = Just (HttpPassword "secret")
    }

httpProxyHost :: Text -> ProxySettings
httpProxyHost host =
  ProxySettings
    { proxySettingsServer = ProxyHost host
    , proxySettingsBypass = Nothing
    , proxySettingsUsername = Nothing
    , proxySettingsPassword = Nothing
    }

-- | Base64 of "user:secret".
expectedAuth :: Maybe BS8.ByteString
expectedAuth = Just "Basic dXNlcjpzZWNyZXQ="

simpleText :: Text
simpleText = "{\"foo\": \"bar\"}\n"

-- | Target app behind the tunnel. Serves simple.json as JSON, anything else as HTML.
targetApp :: IORef [Text] -> Application
targetApp paths request respond = do
  modifyIORef' paths (<> [pack (BS8.unpack (rawPathInfo request))])
  if "simple.json" `BS8.isInfixOf` rawPathInfo request
    then
      respond
        ( responseLBS
            status200
            [(hContentType, BS8.pack "application/json")]
            (LBS8.pack (unpack simpleText))
        )
    else respond (html "Served by the proxy")

-- | Accepts CONNECT. Records the authority and proxy-authorization, then
-- either answers 407 (when auth is required and missing) or tunnels to the target.
proxyApp :: Bool -> Int -> IORef [Text] -> IORef [Maybe BS8.ByteString] -> Application
proxyApp requireAuth targetPort connects auths request respond
  | requestMethod request /= BS8.pack "CONNECT" =
      respond (responseLBS status400 [] (LBS8.pack "expected CONNECT"))
  | otherwise = do
      let
        auth = lookup "proxy-authorization" (requestHeaders request)
        authority = maybe "" (pack . BS8.unpack) (lookup "host" (requestHeaders request))
      modifyIORef' connects (<> [authority])
      modifyIORef' auths (<> [auth])
      case auth of
        Nothing
          | requireAuth ->
              respond proxyAuthRequired
        _ ->
          respond (responseRaw (tunnel targetPort) (responseLBS status200 [] LBS8.empty))

-- | Relays bytes between the client and a local target port until either side closes.
tunnel :: Int -> IO BS8.ByteString -> (BS8.ByteString -> IO ()) -> IO ()
tunnel targetPort fromClient toClient =
  bracket (connectLocal targetPort) close $ \sock -> do
    toClient (BS8.pack "HTTP/1.1 200 Connection Established\r\n\r\n")
    race_ (pumpIn sock) (pumpOut sock)
  where
    pumpIn sock = do
      chunk <- fromClient
      unless (BS8.null chunk) $ do
        NSB.sendAll sock chunk
        pumpIn sock
    pumpOut sock = do
      chunk <- NSB.recv sock 4096
      unless (BS8.null chunk) $ do
        toClient chunk
        pumpOut sock

connectLocal :: Int -> IO Socket
connectLocal port = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
  pure sock

html :: Text -> Response
html titleText =
  responseLBS
    status200
    [(hContentType, BS8.pack "text/html")]
    (LBS8.pack ("<html><title>" <> unpack titleText <> "</title></html>"))

proxyAuthRequired :: Response
proxyAuthRequired =
  responseLBS
    status407
    [("Proxy-Authenticate", BS8.pack "Basic realm=\"proxy\"")]
    (LBS8.pack "Proxy authorization required")

status407 :: Status
status407 =
  mkStatus 407 "Proxy Authentication Required"

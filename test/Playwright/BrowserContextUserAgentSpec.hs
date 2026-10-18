module Playwright.BrowserContextUserAgentSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Control.Concurrent
  ( Chan
  , newChan
  , readChan
  , writeChan
  )
import UnliftIO.Async
  ( withAsync
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.Text
  ( Text
  , isInfixOf
  , pack
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Network.HTTP.Types
  ( RequestHeaders
  , hContentType
  , status200
  )
import Network.HTTP.Types qualified as HTTP
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( AcceptDownloads (AcceptDownloadsAccept)
  , NewContextOptions (newContextAcceptDownloads, newContextHasTouch, newContextIsMobile, newContextUserAgent, newContextViewport)
  , Page
  , UserAgent (UserAgent)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , click
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateBool
  , evaluateText
  , goto
  , setContent
  , urlFromText
  , Playwright
  )
import Playwright.Harness
  ( attachFrame
  , locate
  , withAssetsContextWith
  , withChromiumContext
  , withChromiumContextWith
  , assertBool
  , assertEqual
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "browsercontext-user-agent"
      [ testCase "should work" $
          userAgentWorkTest
      , testCase "should work for subframes" $
          userAgentSubframeTest
      , testCase "should emulate device user-agent" $
          iPhoneUserAgentTest
      , testCase "should make a copy of default options" $
          copyOptionsTest
      , testCase "custom user agent for download" $
          downloadUserAgentTest
      , testCase "should work for navigator.userAgentData and sec-ch-ua headers" $
          userAgentDataTest
      ]

userAgentWorkTest :: IO ()
userAgentWorkTest = do
  withChromiumContext $ \_ page -> do
    ua <- evaluateText page "() => navigator.userAgent"
    assertBool "mozilla" ("Mozilla" `isInfixOf` ua)
  withHeaderCapture defaultNewContextOptions {newContextUserAgent = Just (UserAgent "foobar")} $
    \prefix captured page -> do
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      headers <- liftIO (readChan captured)
      assertEqual "ua" (Just "foobar") (lookupHeader "user-agent" headers)

userAgentSubframeTest :: IO ()
userAgentSubframeTest = do
  withChromiumContext $ \_ page -> do
    ua <- evaluateText page "() => navigator.userAgent"
    assertBool "mozilla" ("Mozilla" `isInfixOf` ua)
  withHeaderCapture defaultNewContextOptions {newContextUserAgent = Just (UserAgent "foobar")} $
    \prefix captured page -> do
      _ <- attachFrame page "frame1" (prefix <> "/empty.html")
      headers <- liftIO (readChan captured)
      assertEqual "ua" (Just "foobar") (lookupHeader "user-agent" headers)

iPhoneUserAgentTest :: IO ()
iPhoneUserAgentTest = do
  withAssetsContextWith defaultNewContextOptions $ \prefix _ page -> do
    _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
    ua <- evaluateText page "() => navigator.userAgent"
    assertBool "not iphone" (not ("iPhone" `isInfixOf` ua))
  withAssetsContextWith
    defaultNewContextOptions {newContextUserAgent = Just (UserAgent iPhoneUserAgent)}
    $ \prefix _ page -> do
      _ <- goto page (urlFromText (prefix <> "/mobile.html")) defaultGotoOptions
      ua <- evaluateText page "() => navigator.userAgent"
      assertBool "iphone" ("iPhone" `isInfixOf` ua)

copyOptionsTest :: IO ()
copyOptionsTest =
  let
    opts = defaultNewContextOptions {newContextUserAgent = Just (UserAgent "foobar")}
  in
    withHeaderCapture opts $ \prefix captured page -> do
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      headers <- liftIO (readChan captured)
      assertEqual "ua" (Just "foobar") (lookupHeader "user-agent" headers)
      assertEqual
        "unchanged"
        (Just (UserAgent "foobar"))
        (newContextUserAgent opts)

downloadUserAgentTest :: IO ()
downloadUserAgentTest = do
  captured <- newChan
  withApplication (pure (downloadApp captured)) $ \port ->
    withChromiumContextWith
      defaultNewContextOptions
        { newContextUserAgent = Just (UserAgent "MyCustomUA")
        , newContextAcceptDownloads = Just AcceptDownloadsAccept
        }
      $ \_ page -> do
        let prefix = "http://127.0.0.1:" <> pack (show port)
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        setContent page "<a id=\"download\" download=\"name\" href=\"/download\">Download</a>"
        withAsync (click (locate page "#download")) $ \_ -> do
          headers <- liftIO (readChan captured)
          assertEqual "ua" (Just "MyCustomUA") (lookupHeader "user-agent" headers)

userAgentDataTest :: IO ()
userAgentDataTest = do
  withHeaderCapture defaultNewContextOptions $ \prefix captured page -> do
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    headers <- liftIO (readChan captured)
    assertBool
      "chromium hint"
      (maybe False ("Chromium" `isInfixOf`) (fmap decodeUtf8 (lookupHeader "sec-ch-ua" headers)))
    assertEqual "not mobile" (Just "?0") (lookupHeader "sec-ch-ua-mobile" headers)
    assertBool "platform" (maybe False (not . ByteString.null) (lookupHeader "sec-ch-ua-platform" headers))
    assertEqual "mobile" False =<< evaluateBool page "() => navigator.userAgentData.mobile"
  withHeaderCapture
    defaultNewContextOptions
      { newContextUserAgent = Just (UserAgent pixelUserAgent)
      , newContextViewport = Just (ViewportSize (ViewportWidth 412) (ViewportHeight 839))
      , newContextIsMobile = Just True
      , newContextHasTouch = Just True
      }
    $ \prefix captured page -> do
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      headers <- liftIO (readChan captured)
      assertBool
        "chromium hint"
        (maybe False ("Chromium" `isInfixOf`) (fmap decodeUtf8 (lookupHeader "sec-ch-ua" headers)))
      assertEqual "mobile hint" (Just "?1") (lookupHeader "sec-ch-ua-mobile" headers)
      assertEqual "android" (Just "\"Android\"") (lookupHeader "sec-ch-ua-platform" headers)
      assertEqual "mobile" True =<< evaluateBool page "() => navigator.userAgentData.mobile"
      assertEqual "platform" "Android" =<< evaluateText page "() => navigator.userAgentData.platform"

iPhoneUserAgent :: Text
iPhoneUserAgent =
  "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/11.0 Mobile/15A372 Safari/604.1"

pixelUserAgent :: Text
pixelUserAgent =
  "Mozilla/5.0 (Linux; Android 14; Pixel 7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.6778.33 Mobile Safari/537.36"

withHeaderCapture :: NewContextOptions -> (Text -> Chan RequestHeaders -> Page -> Playwright a) -> IO a
withHeaderCapture opts action = do
  captured <- newChan
  withApplication (pure (headerCaptureApp captured)) $ \port ->
    withChromiumContextWith opts $ \_ page ->
      action ("http://127.0.0.1:" <> pack (show port)) captured page

headerCaptureApp :: Chan RequestHeaders -> Application
headerCaptureApp captured request respond = do
  writeChan captured (requestHeaders request)
  respond (responseLBS status200 [(hContentType, "text/html")] "<html><body></body></html>")

downloadApp :: Chan RequestHeaders -> Application
downloadApp captured request respond =
  case pathInfo request of
    ["download"] -> do
      writeChan captured (requestHeaders request)
      respond
        ( responseLBS
            status200
            [(hContentType, "application/octet-stream"), ("Content-Disposition", "attachment")]
            "Hello world"
        )
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<html></html>")

lookupHeader :: HTTP.HeaderName -> RequestHeaders -> Maybe ByteString
lookupHeader =
  lookup

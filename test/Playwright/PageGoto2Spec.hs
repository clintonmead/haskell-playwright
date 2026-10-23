module Playwright.PageGoto2Spec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status200, status404)
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( GotoOptions (gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , HttpStatus (httpStatusToInt)
  , LoadState (LoadStateLoad, LoadStateNetworkIdle)
  , Playwright
  , defaultGotoOptions
  , evaluate
  , frames
  , goto
  , onLoad
  , onRequest
  , pageContext
  , requestUrl
  , responseBody
  , responseOk
  , responseStatus
  , responseUrl
  , setContextDefaultTimeout
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , setExtraHTTPHeaders
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
import Playwright.PageGotoSupport
  ( requireMs
  , withHangServer
  , withGotoServer
  , redirectApp
  , refererNavigation
  , canceledNavigation
  , commitNavigation
  , rtcApp
  , loadEventOrder
  , windowStopGoto
  , gotoInterruptedBySecond
  , iframeAttachDetach
  , replacedByAnotherNavigation
  , jsRedirectUrlBar
  , urlBarDuringPending
  )
import Test.Tasty
  ( localOption
  , mkTimeout
  , testGroup
  , TestTree
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( SomeException
  , try
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  , modifyIORef'
  )

tests :: TestTree
tests =
  testGroup
    "page-goto"
    [ testCase "should fail when exceeding default maximum timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix -> do
            pageTimeout <- requireMs 1
            contextTimeout <- requireMs 2
            setContextDefaultTimeout (pageContext page) contextTimeout
            setDefaultTimeout page pageTimeout
            assertProtocolContains
              "Timeout 1ms exceeded"
              (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should fail when exceeding browser context timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix -> do
            timeout <- requireMs 2
            setContextDefaultTimeout (pageContext page) timeout
            assertProtocolContains
              "Timeout 2ms exceeded"
              (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should prioritize default navigation timeout over default timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix -> do
            disabled <- requireMs 0
            navigationTimeout <- requireMs 1
            setDefaultTimeout page disabled
            setDefaultNavigationTimeout page navigationTimeout
            assertProtocolContains
              "Timeout 1ms exceeded"
              (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should disable timeout when its set to 0" $
        withAssets $ \prefix page -> do
          loaded <- newIORef False
          onLoad page (writeIORef loaded True)
          disabled <- requireMs 0
          _ <-
            goto
              page
              (urlFromText (prefix <> "/grid.html"))
              defaultGotoOptions
                { gotoOptionsTimeout = Just disabled
                , gotoOptionsWaitUntil = Just LoadStateLoad
                }
          assertEqual "loaded" True =<< readIORef loaded
    , testCase "should work when navigating to valid url" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should work when navigating to data url" $
        withChromiumPage $ \page -> do
          response <- goto page (urlFromText "data:text/html,hello") defaultGotoOptions
          case response of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no response"
    , testCase "should work when navigating to 404" $
        withAssets $ \prefix page -> do
          let missing = prefix <> "/not-found"
          response <- goto page (urlFromText missing) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "status" 404 . httpStatusToInt =<< responseStatus value
          assertEqual "url" missing . urlToText =<< url page
    , testCase "should return last response in redirect chain" $
        withGotoServer redirectApp $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/redirect/1.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "ok" True =<< responseOk value
              assertEqual "url" (prefix <> "/empty.html") . urlToText =<< responseUrl value
    , testCase "should navigate to dataURL and not fire dataURL requests" $
        withChromiumPage $ \page -> do
          seen <- newIORef (0 :: Int)
          onRequest page $ \_ -> modifyIORef' seen (+ 1)
          response <- goto page (urlFromText "data:text/html,<div>yo</div>") defaultGotoOptions
          case response of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no response"
          assertEqual "requests" 0 =<< readIORef seen
    , testCase "should navigate to URL with hash and fire requests without hash" $
        withAssets $ \prefix page -> do
          let empty = prefix <> "/empty.html"
          seen <- newIORef ([] :: [Text])
          onRequest page $ \request -> do
            location <- urlToText <$> requestUrl request
            modifyIORef' seen (<> [location])
          response <- goto page (urlFromText (empty <> "#hash")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
              assertEqual "response url" empty . urlToText =<< responseUrl value
          assertEqual "requests" [empty] =<< readIORef seen
    , testCase "should work with self requesting page" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/self-request.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
              location <- urlToText <$> responseUrl value
              assertEqual "self" True ("self-request.html" `Text.isInfixOf` location)
    , testCase "should be able to navigate to a page controlled by service worker" $
        withAssets $ \prefix page -> do
          let sw = prefix <> "/serviceworkers/fetch/sw.html"
          _ <- goto page (urlFromText sw) defaultGotoOptions
          _ <- evaluate page "() => window['activationPromise']"
          _ <- goto page (urlFromText sw) defaultGotoOptions
          pure ()
    , testCase "should send referer" $
        refererNavigation "http://google.com/" True
    , testCase "should send referer of cross-origin URL" $
        refererNavigation "https://microsoft.com/xbox/" True
    , testCase "should reject referer option when setExtraHTTPHeaders provides referer" $
        withAssets $ \prefix page -> do
          setExtraHTTPHeaders
            page
            [HttpHeader (HeaderName "referer") (HeaderValue "http://microsoft.com/")]
          assertProtocolContains
            "\"referer\" is already specified as extra HTTP header"
            ( goto
                page
                (urlFromText (prefix <> "/grid.html"))
                defaultGotoOptions {gotoOptionsReferer = Just (urlFromText "http://google.com/")}
            )
    , testCase "should override referrer-policy" $
        refererNavigation "http://microsoft.com/" False
    , testCase "should fail when canceled by another navigation" $
        canceledNavigation
    , testCase "should work with lazy loading iframes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/lazy-frame.html")) defaultGotoOptions
          assertEqual "frames" 2 . length =<< frames page
    , testCase "should not throw unhandled rejections on invalid url" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Panel Title"
            (goto page (urlFromText "https://www.youtube Panel Title.com/") defaultGotoOptions)
    , testCase "should not crash when RTCPeerConnection is used" $
        withGotoServer rtcApp $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rtc.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { window.RTCPeerConnection && new window.RTCPeerConnection({ iceServers: [] }); }"
          pure ()
    , testCase "should return when navigation is committed if commit is specified" $
        commitNavigation
    , testCase "should properly wait for load" $
        loadEventOrder
    , testCase "should return url with basic auth info" $
        withAssets $ \prefix page -> do
          let authed = "http://admin:admin@" <> Text.drop 7 prefix <> "/empty.html"
          _ <- goto page (urlFromText authed) defaultGotoOptions
          assertEqual "url" authed . urlToText =<< url page
    , testCase "should not throw if networkidle is passed" $
        withAssets $ \prefix page -> do
          _ <-
            goto
              page
              (urlFromText (prefix <> "/empty.html"))
              defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateNetworkIdle}
          pure ()
    , testCase "should report raw buffer for main resource" $
        withGotoServer rawBufferApp $ \prefix page -> do
          -- Upstream marks this it.fail on chromium (Chromium sends the main resource as text),
          -- so the case passes while the body still differs from the raw bytes.
          outcome <-
            try
              ( do
                  response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
                  case response of
                    Nothing ->
                      assertFailure "missing response"
                    Just value ->
                      assertEqual "body" rawBufferBytes =<< responseBody value
              )
              :: Playwright (Either SomeException ())
          case outcome of
            Left _ ->
              pure ()
            Right () ->
              assertFailure "main resource body matched the raw buffer; the upstream it.fail has been fixed, so port this as a passing case"
    , localOption (mkTimeout 30000000) $
        testCase "should not resolve goto upon window.stop()" $
          windowStopGoto
    , localOption (mkTimeout 30000000) $
        testCase "should return from goto if new navigation is started" $
          gotoInterruptedBySecond
    , localOption (mkTimeout 30000000) $
        testCase "should wait for load when iframe attaches and detaches" $
          iframeAttachDetach
    , localOption (mkTimeout 30000000) $
        testCase "should fail when replaced by another navigation" $
          replacedByAnotherNavigation
    , localOption (mkTimeout 30000000) $
        testCase "js redirect overrides url bar navigation" $
          jsRedirectUrlBar
    , localOption (mkTimeout 30000000) $
        testCase "should succeed on url bar navigation when there is pending navigation" $
          urlBarDuringPending
    ]

rawBufferBytes :: BS.ByteString
rawBufferBytes =
  encodeUtf8 "Ü (lowercase ü)"

rawBufferApp :: Application
rawBufferApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond (responseLBS status200 [] (LBS.fromStrict rawBufferBytes))
    _ ->
      respond (responseLBS status404 [] "")

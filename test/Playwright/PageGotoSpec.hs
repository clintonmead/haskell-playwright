module Playwright.PageGotoSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  )
import UnliftIO
  ( withRunInIO
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Async
  ( async
  , wait
  , withAsync
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )
import UnliftIO.Exception
  ( SomeException
  , try
  )
import Control.Monad
  ( forever
  , void
  )
import Data.ByteString.Lazy
  ( ByteString
  )
import Data.ByteString.Lazy qualified as LBS
import UnliftIO.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.List
  ( find
  )
import Data.Text
  ( Text
  , pack
  , replace
  , toLower
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Data.Text qualified as Text
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , hReferer
  , status200
  , status204
  , status302
  , status404
  , status500
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  , responseRaw
  )
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Frame
  , GotoOptions (gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , HttpStatus (httpStatusToInt)
  , LoadState (LoadStateCommit, LoadStateDomContentLoaded, LoadStateLoad, LoadStateNetworkIdle)
  , Page
  , Request
  , Response
  , Timeout
  , UrlMatch (UrlMatchGlob)
  , armFrameDetached
  , continueRoute
  , defaultGotoOptions
  , evaluate
  , evaluateTexts
  , frames
  , framesEqual
  , goto
  , mainFrame
  , milliseconds
  , onLoad
  , onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , pageContext
  , querySelector
  , requestFailure
  , requestFrame
  , requestRedirectedFrom
  , requestUrl
  , requestsEqual
  , responseFinished
  , responseFrame
  , responseOk
  , responseRequest
  , responseStatus
  , responseUrl
  , route
  , selectorFromText
  , setContextDefaultNavigationTimeout
  , setContextDefaultTimeout
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , setExtraHTTPHeaders
  , title
  , url
  , urlFromText
  , urlToText
  , waitForTimeout
  , Playwright
  )
import Playwright.Harness
  ( assertProtocolContains
  , readTestAsset
  , testAssetsDir
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertEqual
  , assertFailure
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
  testGroup
    "page-goto"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "url" empty current
    , testCase "page.url should work" $
        withAssets $ \prefix page -> do
          blank <- urlToText <$> url page
          assertEqual "blank" "about:blank" blank
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "empty" empty current
    , testCase "page.url should include hashes" $
        withAssets $ \prefix page -> do
          let
            hashed :: Text
            hashed = prefix <> "/empty.html#hash"
          _ <- goto page (urlFromText hashed) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "hash" hashed current
          _ <- evaluate page "() => { window.location.hash = 'dynamic'; }"
          updated <- urlToText <$> url page
          assertEqual "dynamic" (prefix <> "/empty.html#dynamic") updated
    , testCase "should work with file URL" $
        withChromiumPage $ \page -> do
          fileurl <- assetFileUrl "empty.html"
          _ <- goto page (urlFromText fileurl) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "file" (toLower fileurl) (toLower current)
          assertEqual "frames" 1 . length =<< frames page
    , testCase "should work with file URL with subframes" $
        withChromiumPage $ \page -> do
          fileurl <- assetFileUrl "frames/two-frames.html"
          _ <- goto page (urlFromText fileurl) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "file" (toLower fileurl) (toLower current)
          assertEqual "frames" 3 . length =<< frames page
    , testCase "should use http for no protocol" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText (Text.drop 7 empty)) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "url" empty current
    , testCase "should work cross-process" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
            cross :: Text
            cross = crossProcess prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          seen <- newIORef (Nothing :: Maybe Frame)
          onRequest page $ \request -> do
            location <- urlToText <$> requestUrl request
            if location == cross
              then writeIORef seen =<< requestFrame request
              else pure ()
          response <- goto page (urlFromText cross) defaultGotoOptions
          current <- urlToText <$> url page
          assertEqual "url" cross current
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              frame <- responseFrame value
              case frame of
                Nothing ->
                  assertFailure "missing response frame"
                Just actual ->
                  assertEqual "response frame" True (framesEqual actual (mainFrame page))
              assertEqual "response url" cross . urlToText =<< responseUrl value
          requestFrameValue <- readIORef seen
          case requestFrameValue of
            Nothing ->
              assertFailure "missing request frame"
            Just actual ->
              assertEqual "request frame" True (framesEqual actual (mainFrame page))
    , testCase "should work with cross-process that fails before committing" $
        failBeforeCommit
    , testCase "should work with Cross-Origin-Opener-Policy" $
        coopTest False
    , testCase "should work with Cross-Origin-Opener-Policy and interception" $
        coopTest True
    , testCase "should work with Cross-Origin-Opener-Policy after redirect" $
        coopRedirectTest
    , testCase "should capture iframe navigation request" $
        captureIframeNavigation False
    , testCase "should capture cross-process iframe navigation request" $
        captureIframeNavigation True
    , testCase "should work with anchor navigation" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          assertEqual "empty" empty . urlToText =<< url page
          _ <- goto page (urlFromText (empty <> "#foo")) defaultGotoOptions
          assertEqual "foo" (empty <> "#foo") . urlToText =<< url page
          _ <- goto page (urlFromText (empty <> "#bar")) defaultGotoOptions
          assertEqual "bar" (empty <> "#bar") . urlToText =<< url page
    , testCase "should work with redirects" $
        withGotoServer redirectApp $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/redirect/1.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
          assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url page
    , testCase "should navigate to about:blank" $
        withChromiumPage $ \page -> do
          response <- goto page (urlFromText "about:blank") defaultGotoOptions
          case response of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no response"
    , testCase "should return response when page changes its URL after load" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/historyapi.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    , testCase "should work with subframes return 204" $
        subframe204 Nothing
    , testCase "should work with subframes return 204 with domcontentloaded" $
        subframe204 (Just LoadStateDomContentLoaded)
    , testCase "should fail when server returns 204" $
        withGotoServer empty204App $ \prefix page ->
          assertProtocolContains
            "net::ERR_ABORTED"
            (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should navigate to empty page with domcontentloaded" $
        withAssets $ \prefix page -> do
          response <-
            goto
              page
              (urlFromText (prefix <> "/empty.html"))
              defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateDomContentLoaded}
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    , testCase "should work when page calls history API in beforeunload" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { window.addEventListener('beforeunload', () => history.replaceState(null, 'initial', window.location.href), false); }"
          response <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    , testCase "should fail when navigating to bad url" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Cannot navigate to invalid URL"
            (goto page (urlFromText "asdfasdf") defaultGotoOptions)
    , testCase "should fail when main resources failed to load" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "net::ERR_CONNECTION_REFUSED"
            (goto page (urlFromText "http://localhost:44123/non-existing-url") defaultGotoOptions)
    , testCase "should fail when exceeding maximum navigation timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix -> do
            timeout <- requireMs 1
            assertProtocolContains
              "Timeout 1ms exceeded"
              ( goto
                  page
                  (urlFromText (prefix <> "/empty.html"))
                  defaultGotoOptions {gotoOptionsTimeout = Just timeout}
              )
    , testCase "should fail when exceeding default maximum navigation timeout" $
        withChromiumPage $ \page ->
          withHangServer $ \prefix -> do
            timeout <- requireMs 2
            setContextDefaultNavigationTimeout (pageContext page) timeout
            assertProtocolContains
              "Timeout 2ms exceeded"
              (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
    , testCase "should fail when exceeding default maximum timeout" $
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

requireMs :: (MonadIO m) => Double -> m Timeout
requireMs n =
  case milliseconds n of
    Nothing ->
      assertFailure ("timeout " <> show n)
    Just timeout ->
      pure timeout

assetFileUrl :: (MonadIO m) => FilePath -> m Text
assetFileUrl relative = do
  dir <- testAssetsDir
  pure ("file://" <> pack dir <> "/" <> pack relative)

crossProcess :: Text -> Text
crossProcess =
  replace "127.0.0.1" "localhost"

withHangServer :: (Text -> Playwright a) -> Playwright a
withHangServer =
  withLocalApp hangApp

withGotoServer :: Application -> (Text -> Page -> Playwright a) -> IO a
withGotoServer app action =
  withChromiumPage $ \page ->
    withLocalApp app $ \prefix ->
      action prefix page

hangApp :: Application
hangApp _request _respond =
  forever (threadDelay 1000000)

empty204App :: Application
empty204App _request respond =
  respond (responseLBS status204 [] "")

redirectApp :: Application
redirectApp request respond =
  case pathInfo request of
    ["redirect", "1.html"] ->
      respond (responseLBS status302 [(hLocation, "/redirect/2.html")] "")
    ["redirect", "2.html"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ ->
      respond (responseLBS status404 [] "")

coopHtml :: ByteString
coopHtml =
  "<div>Hello there!</div><script>window.onload = () => console.log('onload')</script>"

coopApp :: Application
coopApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/html")
            , ("Cross-Origin-Opener-Policy", "same-origin")
            ]
            coopHtml
        )
    _ ->
      respond (responseLBS status404 [] "")

coopRedirectApp :: Application
coopRedirectApp request respond =
  case pathInfo request of
    ["redirect"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [ (hContentType, "text/html")
            , ("Cross-Origin-Opener-Policy", "same-origin")
            ]
            coopHtml
        )
    _ ->
      respond (responseLBS status404 [] "")

listenNetwork :: Page -> Playwright (IORef [Text], IORef [Request])
listenNetwork page = do
  events <- newIORef ([] :: [Text])
  seen <- newIORef ([] :: [Request])
  let
    remember :: Text -> Request -> Playwright ()
    remember label request = do
      modifyIORef' events (<> [label])
      modifyIORef' seen (addUnique request)
  onRequest page (remember "request")
  onRequestFailed page (remember "requestfailed")
  onRequestFinished page (remember "requestfinished")
  onResponse page $ \response -> do
    request <- responseRequest response
    remember "response" request
  pure (events, seen)

addUnique :: Request -> [Request] -> [Request]
addUnique request seen =
  if any (requestsEqual request) seen
    then seen
    else seen <> [request]

coopTest :: Bool -> IO ()
coopTest intercept =
  withGotoServer coopApp $ \prefix page -> do
    (events, seen) <- listenNetwork page
    if intercept
      then
        route page (UrlMatchGlob "**/*") $ \handled -> do
          threadDelay 100000
          continueRoute handled
      else
        pure ()
    response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url page
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value -> do
        responseFinished value
        assertEqual "events" ["request", "response", "requestfinished"] =<< readIORef events
        assertEqual "unique" 1 . length =<< readIORef seen
        failure <- requestFailure =<< responseRequest value
        case failure of
          Nothing ->
            pure ()
          Just _ ->
            assertFailure "expected no failure"

coopRedirectTest :: IO ()
coopRedirectTest =
  withGotoServer coopRedirectApp $ \prefix page -> do
    (events, seen) <- listenNetwork page
    response <- goto page (urlFromText (prefix <> "/redirect")) defaultGotoOptions
    assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url page
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value -> do
        responseFinished value
        assertEqual
          "events"
          ["request", "response", "requestfinished", "request", "response", "requestfinished"]
          =<< readIORef events
        assertEqual "unique" 2 . length =<< readIORef seen
        failure <- requestFailure =<< responseRequest value
        case failure of
          Nothing ->
            pure ()
          Just _ ->
            assertFailure "expected no failure"
        redirected <- requestRedirectedFrom =<< responseRequest value
        case redirected of
          Nothing ->
            assertFailure "expected redirectedFrom"
          Just first ->
            assertEqual "redirect url" (prefix <> "/redirect") . urlToText =<< requestUrl first

captureIframeNavigation :: Bool -> IO ()
captureIframeNavigation cross =
  withAssets $ \prefix page -> do
    let
      empty :: Text
      empty = prefix <> "/empty.html"
      origin :: Text
      origin =
        if cross
          then crossProcess prefix
          else prefix
      frameUrl :: Text
      frameUrl = origin <> "/frames/frame.html"
      pageUrl :: Text
      pageUrl = origin <> "/frames/one-frame.html"
    _ <- goto page (urlFromText empty) defaultGotoOptions
    seen <- newIORef (Nothing :: Maybe Frame)
    onRequest page $ \request -> do
      location <- urlToText <$> requestUrl request
      if location == frameUrl
        then writeIORef seen =<< requestFrame request
        else pure ()
    response <- goto page (urlFromText pageUrl) defaultGotoOptions
    assertEqual "url" pageUrl . urlToText =<< url page
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value -> do
        frame <- responseFrame value
        case frame of
          Nothing ->
            assertFailure "missing response frame"
          Just actual ->
            assertEqual "response frame" True (framesEqual actual (mainFrame page))
        assertEqual "response url" pageUrl . urlToText =<< responseUrl value
    allFrames <- frames page
    assertEqual "frames" 2 (length allFrames)
    requestFrameValue <- readIORef seen
    case (requestFrameValue, allFrames) of
      (Just actual, [_, iframe]) ->
        assertEqual "iframe" True (framesEqual actual iframe)
      (Nothing, _) ->
        assertFailure "missing iframe request"
      _ ->
        assertFailure "expected two frames"

subframe204 :: Maybe LoadState -> IO ()
subframe204 waitUntil = do
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app request respond =
      if pathInfo request == ["frames", "frame.html"]
        then respond (responseLBS status204 [] "")
        else static request respond
  withGotoServer app $ \prefix page -> do
    _ <-
      goto
        page
        (urlFromText (prefix <> "/frames/one-frame.html"))
        defaultGotoOptions {gotoOptionsWaitUntil = waitUntil}
    pure ()

refererNavigation :: Text -> Bool -> IO ()
refererNavigation referer inheritPng = do
  hits <- newIORef ([] :: [(Text, Maybe Text)])
  assets <- testAssetsDir
  grid <- readTestAsset "grid.html"
  let
    static = staticApp (defaultFileServerSettings assets)
    app request respond = do
      modifyIORef' hits (<> [(pathKey request, headerReferer request)])
      if pathInfo request == ["grid.html"] && not inheritPng
        then
          respond
            ( responseLBS
                status200
                [ (hContentType, "text/html")
                , ("Referrer-Policy", "no-referrer")
                ]
                (LBS.fromStrict grid)
            )
        else
          static request respond
  withGotoServer app $ \prefix page -> do
    let gridUrl = prefix <> "/grid.html"
    _ <-
      goto
        page
        (urlFromText gridUrl)
        defaultGotoOptions {gotoOptionsReferer = Just (urlFromText referer)}
    recorded <- readIORef hits
    case firstReferer "grid.html" recorded of
      Nothing ->
        assertFailure "missing grid request"
      Just actual ->
        assertEqual "grid" (Just referer) actual
    case firstReferer "digits/1.png" recorded of
      Nothing ->
        assertFailure "missing png request"
      Just actual ->
        if inheritPng
          then assertEqual "png" (Just gridUrl) actual
          else assertEqual "png" Nothing actual
    assertEqual "url" gridUrl . urlToText =<< url page

pathKey :: Wai.Request -> Text
pathKey request =
  Text.intercalate "/" (pathInfo request)

headerReferer :: Wai.Request -> Maybe Text
headerReferer request =
  fmap decodeUtf8 (lookup hReferer (requestHeaders request))

firstReferer :: Text -> [(Text, Maybe Text)] -> Maybe (Maybe Text)
firstReferer path hits =
  fmap snd (find (\(name, _) -> name == path) hits)

canceledNavigation :: IO ()
canceledNavigation = do
  started <- newEmptyMVar
  let
    app :: Application
    app request respond =
      case pathInfo request of
        ["one-style.html"] -> do
          putMVar started ()
          forever (threadDelay 1000000)
        ["empty.html"] ->
          respond (responseLBS status200 [(hContentType, "text/html")] "")
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page ->
    withAsync
      ( try (goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions) ::
          Playwright (Either SomeException (Maybe Response))
      )
      $ \pending -> do
        takeMVar started
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        result <- wait pending
        case result of
          Left _ ->
            pure ()
          Right _ ->
            assertFailure "expected canceled navigation to fail"

commitNavigation :: IO ()
commitNavigation =
  withGotoServer commitApp $ \prefix page -> do
    response <-
      goto
        page
        (urlFromText (prefix <> "/empty.html"))
        defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateCommit}
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value ->
        assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    assertEqual "title" "Hello" =<< title page

commitApp :: Application
commitApp request respond =
  case pathInfo request of
    ["script.js"] ->
      forever (threadDelay 1000000)
    ["empty.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<title>Hello</title><script src=\"script.js\"></script>"
        )
    _ ->
      respond (responseLBS status404 [] "")

rtcApp :: Application
rtcApp request respond =
  case pathInfo request of
    ["rtc.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<!DOCTYPE html><html><body><script>window.RTCPeerConnection && new window.RTCPeerConnection({ iceServers: [] });</script></body></html>"
        )
    _ ->
      respond (responseLBS status404 [] "")

loadEventOrder :: IO ()
loadEventOrder = do
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app request respond =
      case pathInfo request of
        ["slow.js"] -> do
          threadDelay 100000
          respond
            ( responseLBS
                status200
                [(hContentType, "application/javascript")]
                "window.results.push('slow module');export const foo = 'slow';"
            )
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/load-event/load-event.html")) defaultGotoOptions
    results <- evaluateTexts page "() => window.results"
    assertEqual
      "order"
      [ "script tag after after module"
      , "slow module"
      , "module"
      , "DOMContentLoaded"
      , "load"
      ]
      results

failBeforeCommit :: IO ()
failBeforeCommit = do
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["empty.html"] ->
          respond (responseRaw (\_ _ -> pure ()) (responseLBS status500 [] ""))
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    response <-
      goto page (urlFromText (crossProcess prefix <> "/title.html")) defaultGotoOptions
    case response of
      Nothing ->
        assertFailure "missing response"
      Just value ->
        responseFinished value
    result <-
      try (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) ::
        Playwright (Either SomeException (Maybe Response))
    case result of
      Left _ ->
        pure ()
      Right _ ->
        assertFailure "expected navigation error"

windowStopGoto :: IO ()
windowStopGoto = do
  assets <- testAssetsDir
  started <- newEmptyMVar
  release <- newEmptyMVar
  done <- newIORef False
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["module.js"] -> do
          putMVar started ()
          takeMVar release
          respond
            ( responseLBS
                status200
                [(hContentType, "application/javascript")]
                ""
            )
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    timeout <- requireMs 1000
    withAsync
      ( do
          result <-
            try (goto page (urlFromText (prefix <> "/window-stop.html")) defaultGotoOptions) ::
              Playwright (Either SomeException (Maybe Response))
          case result of
            Left _ ->
              pure ()
            Right _ ->
              writeIORef done True
      )
      $ \_ -> do
        takeMVar started
        assertEqual "before end" False =<< readIORef done
        waitForTimeout page timeout
        putMVar release ()
        waitForTimeout page timeout
        assertEqual "after end" False =<< readIORef done

gotoInterruptedBySecond :: IO ()
gotoInterruptedBySecond = do
  assets <- testAssetsDir
  finished <- newIORef False
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["slow.js"] ->
          forever (threadDelay 1000000)
        _ ->
          static request respond
  withGotoServer app $ \prefix page -> do
    withAsync
      ( do
          response <-
            goto
              page
              (urlFromText (prefix <> "/load-event/load-event.html"))
              defaultGotoOptions
          writeIORef finished True
          pure response
      )
      $ \pending -> do
        threadDelay 500000
        assertEqual "still pending" False =<< readIORef finished
        _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
        first <- wait pending
        case first of
          Nothing ->
            assertFailure "missing first navigation response"
          Just value ->
            assertEqual "status" 200 . httpStatusToInt =<< responseStatus value

iframeAttachDetach :: IO ()
iframeAttachDetach = do
  let
    app :: Application
    app request respond =
      case pathInfo request of
        ["empty.html"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<body><script>const iframe = document.createElement('iframe'); iframe.src = './iframe.html'; document.body.appendChild(iframe); setTimeout(() => iframe.remove(), 1000);</script></body>"
            )
        ["iframe.html"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<link rel=\"stylesheet\" href=\"./style2.css\">"
            )
        ["style2.css"] ->
          forever (threadDelay 1000000)
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page -> do
    awaitDetached <- armFrameDetached page
    withAsync
      ( goto
          page
          (urlFromText (prefix <> "/empty.html"))
          defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateLoad}
      )
      $ \done -> do
        _ <- awaitDetached
        _ <- wait done
        iframe <- querySelector page (selectorFromText "iframe")
        case iframe of
          Nothing ->
            pure ()
          Just _ ->
            assertFailure "expected iframe to be gone"

replacedByAnotherNavigation :: IO ()
replacedByAnotherNavigation = do
  assets <- testAssetsDir
  prefixRef <- newIORef ("" :: Text)
  pageRef <- newIORef (Nothing :: Maybe Page)
  gotoIO <- newIORef (Nothing :: Maybe (Page -> Text -> IO (Maybe Response)))
  another <- newEmptyMVar
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["empty.html"] -> do
          prefix <- readIORef prefixRef
          maybePage <- readIORef pageRef
          maybeGoto <- readIORef gotoIO
          case (maybePage, maybeGoto) of
            (Just page, Just runGoto) -> do
              void $
                async $
                  putMVar another
                    =<< runGoto page (prefix <> "/one-style.html")
              forever (threadDelay 1000000)
            _ ->
              respond (responseLBS status500 [] "")
        _ ->
          static request respond
  withChromiumPage $ \page ->
    withRunInIO $ \run -> do
      writeIORef
        gotoIO
        (Just (\targetPage target -> run (goto targetPage (urlFromText target) defaultGotoOptions)))
      writeIORef pageRef (Just page)
      withApplication (pure app) $ \port ->
        run $ do
          let prefix = "http://127.0.0.1:" <> pack (show port)
          writeIORef prefixRef prefix
          result <-
            try (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) ::
              Playwright (Either SomeException (Maybe Response))
          _ <- takeMVar another
          case result of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected replaced navigation to fail"

jsRedirectUrlBar :: IO ()
jsRedirectUrlBar = do
  events <- newIORef ([] :: [Text])
  let
    record :: Text -> IO ()
    record label =
      atomicModifyIORef' events (\xs -> (xs <> [label], ()))
    app :: Application
    app request respond =
      case pathInfo request of
        ["a"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<body><script>setTimeout(() => { window.location.pathname = '/c'; }, 1000);</script></body>"
            )
        ["b"] -> do
          record "started b"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "BBB")
          record "finished b"
          pure received
        ["c"] -> do
          record "started c"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "CCC")
          record "finished c"
          pure received
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/a")) defaultGotoOptions
    result <-
      try (goto page (urlFromText (prefix <> "/b")) defaultGotoOptions) ::
        Playwright (Either SomeException (Maybe Response))
    recorded <- readIORef events
    assertEqual "events" ["started b", "finished b"] recorded
    case result of
      Left _ ->
        assertFailure "chromium should keep url-bar navigation"
      Right _ ->
        assertEqual "url" (prefix <> "/b") . urlToText =<< url page

urlBarDuringPending :: IO ()
urlBarDuringPending = do
  events <- newIORef ([] :: [Text])
  let
    record :: Text -> IO ()
    record label =
      atomicModifyIORef' events (\xs -> (xs <> [label], ()))
    app :: Application
    app request respond =
      case pathInfo request of
        ["a"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "<body><script>setTimeout(() => { window.location.pathname = '/c'; }, 10);</script></body>"
            )
        ["b"] -> do
          record "started b"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "BBB")
          record "finished b"
          pure received
        ["c"] -> do
          record "started c"
          threadDelay 2000000
          received <- respond (responseLBS status200 [(hContentType, "text/html")] "CCC")
          record "finished c"
          pure received
        _ ->
          respond (responseLBS status404 [] "")
  withGotoServer app $ \prefix page -> do
    timeout <- requireMs 1000
    _ <- goto page (urlFromText (prefix <> "/a")) defaultGotoOptions
    waitForTimeout page timeout
    result <-
      try (goto page (urlFromText (prefix <> "/b")) defaultGotoOptions) ::
        Playwright (Either SomeException (Maybe Response))
    recorded <- readIORef events
    assertEqual "events" ["started c", "started b", "finished c", "finished b"] recorded
    case result of
      Left _ ->
        assertFailure "expected url-bar navigation to succeed"
      Right _ ->
        assertEqual "url" (prefix <> "/b") . urlToText =<< url page


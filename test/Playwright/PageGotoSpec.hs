module Playwright.PageGotoSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forever
  )
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.ByteString.Lazy
  ( ByteString
  )
import Data.Text
  ( Text
  , pack
  , replace
  , toLower
  )
import Data.Text qualified as Text
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , status200
  , status204
  , status302
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( Frame
  , GotoOptions (gotoOptionsTimeout, gotoOptionsWaitUntil)
  , HttpStatus (httpStatusToInt)
  , LoadState (LoadStateDomContentLoaded, LoadStateLoad)
  , Page
  , Request
  , Timeout
  , UrlGlob (UrlGlob)
  , continueRoute
  , defaultGotoOptions
  , evaluate
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
  , setContextDefaultNavigationTimeout
  , setContextDefaultTimeout
  , setDefaultNavigationTimeout
  , setDefaultTimeout
  , url
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertProtocolContains
  , testAssetsDir
  , withAssets
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
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
    ]

requireMs :: Double -> IO Timeout
requireMs n =
  case milliseconds n of
    Nothing ->
      assertFailure ("timeout " <> show n)
    Just timeout ->
      pure timeout

assetFileUrl :: FilePath -> IO Text
assetFileUrl relative = do
  dir <- testAssetsDir
  pure ("file://" <> pack dir <> "/" <> pack relative)

crossProcess :: Text -> Text
crossProcess =
  replace "127.0.0.1" "localhost"

withHangServer :: (Text -> IO a) -> IO a
withHangServer action =
  withApplication (pure hangApp) $ \port ->
    action ("http://127.0.0.1:" <> pack (show port))

withGotoServer :: Application -> (Text -> Page -> IO a) -> IO a
withGotoServer app action =
  withChromiumPage $ \page ->
    withApplication (pure app) $ \port ->
      action ("http://127.0.0.1:" <> pack (show port)) page

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

listenNetwork :: Page -> IO (IORef [Text], IORef [Request])
listenNetwork page = do
  events <- newIORef ([] :: [Text])
  seen <- newIORef ([] :: [Request])
  let
    remember :: Text -> Request -> IO ()
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
        route page (UrlGlob "**/*") $ \handled -> do
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

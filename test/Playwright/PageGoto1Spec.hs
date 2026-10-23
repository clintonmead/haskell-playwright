module Playwright.PageGoto1Spec
  ( tests
  ) where

import Data.Text
  ( Text
  , toLower
  )
import Data.Text qualified as Text
import Playwright
  ( Frame
  , GotoOptions (gotoOptionsTimeout, gotoOptionsWaitUntil)
  , HttpStatus (httpStatusToInt)
  , LoadState (LoadStateDomContentLoaded)
  , defaultGotoOptions
  , evaluate
  , frames
  , framesEqual
  , goto
  , mainFrame
  , onRequest
  , pageContext
  , requestFrame
  , requestUrl
  , responseFrame
  , responseStatus
  , responseUrl
  , setContextDefaultNavigationTimeout
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
  , assetFileUrl
  , crossProcess
  , withHangServer
  , withGotoServer
  , empty204App
  , redirectApp
  , coopTest
  , coopRedirectTest
  , captureIframeNavigation
  , subframe204
  , failBeforeCommit
  )
import Test.Tasty
  ( testGroup
  , TestTree
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
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
    , testCase "should navigate from file URL to about:blank" $
        withChromiumPage $ \page -> do
          fileUrl <- assetFileUrl "empty.html"
          _ <- goto page (urlFromText fileUrl) defaultGotoOptions
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
    ]

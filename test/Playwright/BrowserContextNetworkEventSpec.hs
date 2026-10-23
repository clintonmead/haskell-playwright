module Playwright.BrowserContextNetworkEventSpec
  ( tests
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as LBS
import Control.Concurrent
  ( threadDelay
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Exception
  ( try
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
  , status500
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  , responseStream
  , responseRaw
  )
import Playwright
  ( LoadState (LoadStateLoad)
  , Playwright
  , UrlMatch (UrlMatchGlob)
  , armResponse
  , closeContext
  , evaluateText
  , responseFinished
  , Request
  , RequestFailure (requestFailureText)
  , ResourceType (resourceTypeToText)
  , UrlMatch (UrlMatchGlob)
  , click
  , currentUrl
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluate
  , framesEqual
  , goto
  , mainFrame
  , onContextRequest
  , onContextRequestFailed
  , onContextRequestFinished
  , onContextResponse
  , requestFailure
  , requestFrame
  , requestResourceType
  , requestResponse
  , requestUrl
  , requestsEqual
  , responseRequest
  , responseUrl
  , setContent
  , urlFromText
  , urlToText
  , armContextPage
  , waitForContextRequestFinished
  , waitForLoadState
  , armRequest
  , waitForResponse
  )
import Playwright.Harness
  ( locate
  , readTestAsset
  , withAssetsContextWith
  , withChromiumContext
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-network-event"
    [ testCase "BrowserContext.Events.Request" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          let empty = prefix <> "/empty.html"
          seen <- newIORef ([] :: [Text])
          onContextRequest ctx $ \request -> do
            url <- urlToText <$> requestUrl request
            modifyIORef' seen (<> [url])
          _ <- goto page (urlFromText empty) defaultGotoOptions
          setContent page "<a target=_blank rel=noopener href=\"/one-style.html\">yo</a>"
          armContextPage ctx >>= \pending ->
            armRequest page (UrlMatchGlob "**/one-style.css") >>= \cssPending -> do
              click (locate page "a")
              other <- pending
              waitForLoadState other LoadStateLoad
              _ <- cssPending
              _ <- evaluate page "() => 0"
              pure ()
          urls <- readIORef seen
          assertEqual
            "urls"
            [empty, prefix <> "/one-style.html", prefix <> "/one-style.css"]
            urls
    , testCase "BrowserContext.Events.Response" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          let empty = prefix <> "/empty.html"
          seen <- newIORef ([] :: [Text])
          onContextResponse ctx $ \response -> do
            url <- urlToText <$> responseUrl response
            modifyIORef' seen (<> [url])
          _ <- goto page (urlFromText empty) defaultGotoOptions
          setContent page "<a target=_blank rel=noopener href=\"/one-style.html\">yo</a>"
          armContextPage ctx >>= \pending ->
            withAsync (waitForResponse page (UrlMatchGlob "**/one-style.css")) $ \cssPending -> do
              click (locate page "a")
              other <- pending
              waitForLoadState other LoadStateLoad
              _ <- wait cssPending
              _ <- evaluate page "() => 0"
              pure ()
          urls <- readIORef seen
          assertEqual
            "urls"
            [empty, prefix <> "/one-style.html", prefix <> "/one-style.css"]
            urls
    , testCase "BrowserContext.Events.RequestFailed" $
        withChromiumContext $ \ctx page -> do
          html <- readTestAsset "one-style.html"
          withLocalApp (failedCssApp html) $ \prefix -> do
            failed <- newIORef ([] :: [Request])
            onContextRequestFailed ctx $ \request -> modifyIORef' failed (<> [request])
            _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
            requests <- readIORef failed
            case requests of
              [request] -> do
                assertBool "css" . ("one-style.css" `isInfixOf`) . urlToText =<< requestUrl request
                maybeResponse <- requestResponse request
                case maybeResponse of
                  Nothing ->
                    pure ()
                  Just _ ->
                    assertFailure "failed request should have no response"
                assertEqual "type" "stylesheet" . resourceTypeToText =<< requestResourceType request
                frame <- requestFrame request
                case frame of
                  Nothing ->
                    assertFailure "missing frame"
                  Just _ ->
                    pure ()
              _ ->
                assertFailure ("expected one failed request, got " <> show (length requests))
    , testCase "BrowserContext.Events.RequestFinished" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          let empty = prefix <> "/empty.html"
          withAsync (waitForContextRequestFinished ctx) $ \pending -> do
            response <- goto page (urlFromText empty) defaultGotoOptions
            finished <- wait pending
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                assertEqual "url" empty . urlToText =<< requestUrl request
                maybeResponse <- requestResponse request
                case maybeResponse of
                  Nothing ->
                    assertFailure "missing request response"
                  Just _ ->
                    pure ()
                frame <- requestFrame request
                case frame of
                  Nothing ->
                    assertFailure "missing frame"
                  Just frameValue -> do
                    assertEqual "main" True (framesEqual frameValue (mainFrame page))
                    assertEqual "frame url" empty . urlToText =<< currentUrl frameValue
                failure <- requestFailure request
                case failure of
                  Nothing ->
                    pure ()
                  Just err ->
                    assertFailure ("expected no failure: " <> show (requestFailureText err))
                assertEqual "same" True (requestsEqual request finished)
    , testCase "should fire events in proper order" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          let empty = prefix <> "/empty.html"
          events <- newIORef ([] :: [Text])
          onContextRequest ctx $ \_ -> modifyIORef' events (<> ["request"])
          onContextResponse ctx $ \_ -> modifyIORef' events (<> ["response"])
          onContextRequestFinished ctx $ \_ -> modifyIORef' events (<> ["requestfinished"])
          withAsync (waitForContextRequestFinished ctx) $ \pending -> do
            _ <- goto page (urlFromText empty) defaultGotoOptions
            _ <- wait pending
            pure ()
          assertEqual "order" ["request", "response", "requestfinished"] =<< readIORef events
    , testCase "should reject response.finished if context closes" $
        withChromiumContext $ \ctx page ->
          withLocalApp streamingGetApp $ \base -> do
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            takeResponse <- armResponse page (UrlMatchGlob "**/get")
            _ <- evaluateText page "() => { fetch('./get'); return 'sent'; }"
            response <- takeResponse
            withAsync (try (responseFinished response)) $ \finishing -> do
              closeContext ctx
              result <- wait finishing :: Playwright (Either PlaywrightError ())
              case result of
                Left (ProtocolRpc failure) ->
                  assertBool "closed" ("closed" `isInfixOf` protocolFailureMessage failure)
                Left err ->
                  assertFailure ("expected ProtocolRpc, got: " <> show err)
                Right _ ->
                  assertFailure "expected response.finished to fail"
    ]

failedCssApp :: ByteString -> Application
failedCssApp html request respond =
  case pathInfo request of
    ["one-style.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] (LBS.fromStrict html))
    ["one-style.css"] ->
      respond (responseRaw (\_ _ -> pure ()) (responseLBS status500 [] ""))
    _ ->
      respond (responseLBS status404 [] "")

-- | Answers /get with the start of a body and then waits, so the response never
-- finishes. Every other path answers with an empty page.
streamingGetApp :: Application
streamingGetApp request respond =
  case pathInfo request of
    ["get"] ->
      respond
        ( responseStream
            status200
            [(hContentType, "text/plain; charset=utf-8")]
            ( \write flush -> do
                write (B.byteString "hello ")
                flush
                threadDelay 30000000
            )
        )
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")

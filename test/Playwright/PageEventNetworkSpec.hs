module Playwright.PageEventNetworkSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Lazy qualified as LBS
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , status200
  , status302
  , status404
  , status500
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  , responseRaw
  )
import Playwright
  ( HttpMethod (httpMethodToText)
  , HttpStatus (httpStatusToInt)
  , Playwright
  , Request
  , RequestFailure (requestFailureText)
  , ResourceType (resourceTypeToText)
  , currentUrl
  , defaultGotoOptions
  , framesEqual
  , goto
  , mainFrame
  , onRequest
  , onRequestFailed
  , onRequestFinished
  , onResponse
  , requestFailure
  , requestFrame
  , requestMethod
  , requestRedirectedFrom
  , requestRedirectedTo
  , requestResourceType
  , requestResponse
  , requestUrl
  , requestsEqual
  , responseFinished
  , responseOk
  , responseRequest
  , responseStatus
  , responseUrl
  , urlFromText
  , urlToText
  , waitForRequestFinished
  )
import Playwright.Harness
  ( readTestAsset
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
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
    "page-event-network"
    [ testCase "Page.Events.Request" $
        withAssets $ \prefix page -> do
          let empty = prefix <> "/empty.html"
          seen <- newIORef ([] :: [Request])
          onRequest page $ \request -> modifyIORef' seen (<> [request])
          _ <- goto page (urlFromText empty) defaultGotoOptions
          requests <- readIORef seen
          case requests of
            [request] -> do
              assertEqual "url" empty . urlToText =<< requestUrl request
              assertEqual "type" "document" . resourceTypeToText =<< requestResourceType request
              assertEqual "method" "GET" . httpMethodToText =<< requestMethod request
              maybeResponse <- requestResponse request
              case maybeResponse of
                Nothing ->
                  assertFailure "missing response"
                Just _ ->
                  pure ()
              frame <- requestFrame request
              case frame of
                Nothing ->
                  assertFailure "missing frame"
                Just value -> do
                  assertEqual "main" True (framesEqual value (mainFrame page))
                  assertEqual "frame url" empty . urlToText =<< currentUrl value
            _ ->
              assertFailure ("expected one request, got " <> show (length requests))
    , testCase "Page.Events.Response" $
        withAssets $ \prefix page -> do
          let empty = prefix <> "/empty.html"
          seen <- newIORef (0 :: Int)
          onResponse page $ \_ -> modifyIORef' seen (+ 1)
          response <- goto page (urlFromText empty) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing goto response"
            Just value -> do
              assertEqual "url" empty . urlToText =<< responseUrl value
              assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
              assertEqual "ok" True =<< responseOk value
              _ <- responseRequest value
              pure ()
          assertEqual "count" 1 =<< readIORef seen
    , testCase "Page.Events.RequestFailed" $
        withChromiumPage $ \page -> do
          html <- readTestAsset "one-style.html"
          withLocalApp (failedCssApp html) $ \prefix -> do
            failed <- newIORef ([] :: [Request])
            onRequestFailed page $ \request -> modifyIORef' failed (<> [request])
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
                failure <- requestFailure request
                case failure of
                  Nothing ->
                    assertFailure "missing failure"
                  Just value ->
                    assertEqual "text" "net::ERR_EMPTY_RESPONSE" (requestFailureText value)
                frame <- requestFrame request
                case frame of
                  Nothing ->
                    assertFailure "missing frame"
                  Just _ ->
                    pure ()
              _ ->
                assertFailure ("expected one failed request, got " <> show (length requests))
    , testCase "Page.Events.RequestFinished" $
        withAssets $ \prefix page -> do
          let empty = prefix <> "/empty.html"
          withAsync (waitForRequestFinished page) $ \pending -> do
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
                  Just _ ->
                    assertFailure "expected no failure"
                assertEqual "same" True (requestsEqual request finished)
    , testCase "should fire events in proper order" $
        withAssets $ \prefix page -> do
          let empty = prefix <> "/empty.html"
          events <- newIORef ([] :: [Text])
          onRequest page $ \_ -> modifyIORef' events (<> ["request"])
          onResponse page $ \_ -> modifyIORef' events (<> ["response"])
          response <- goto page (urlFromText empty) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              responseFinished value
          modifyIORef' events (<> ["requestfinished"])
          assertEqual "order" ["request", "response", "requestfinished"] =<< readIORef events
    , testCase "should support redirects" $
        withChromiumPage $ \page -> do
          withLocalApp redirectApp $ \prefix -> do
            let
              empty = prefix <> "/empty.html"
              foo = prefix <> "/foo.html"
            events <- newIORef ([] :: [(Text, Text)])
            onRequest page $ \request -> do
              url <- urlToText <$> requestUrl request
              method <- httpMethodToText <$> requestMethod request
              appendEvent events url method
            onResponse page $ \response -> do
              url <- urlToText <$> responseUrl response
              status <- pack . show . httpStatusToInt <$> responseStatus response
              appendEvent events url status
            onRequestFinished page $ \request -> do
              url <- urlToText <$> requestUrl request
              appendEvent events url "DONE"
            onRequestFailed page $ \request -> do
              url <- urlToText <$> requestUrl request
              appendEvent events url "FAIL"
            response <- goto page (urlFromText foo) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                responseFinished value
                recorded <- readIORef events
                assertEqual "foo" ["GET", "302", "DONE"] (eventsFor foo recorded)
                assertEqual "empty" ["GET", "200", "DONE"] (eventsFor empty recorded)
                request <- responseRequest value
                fromRequest <- requestRedirectedFrom request
                case fromRequest of
                  Nothing ->
                    assertFailure "missing redirectedFrom"
                  Just original -> do
                    assertBool "foo url" . ("/foo.html" `isInfixOf`) . urlToText =<< requestUrl original
                    further <- requestRedirectedFrom original
                    case further of
                      Nothing ->
                        pure ()
                      Just _ ->
                        assertFailure "expected no further redirect"
                    toRequest <- requestRedirectedTo original
                    case toRequest of
                      Nothing ->
                        assertFailure "missing redirectedTo"
                      Just next ->
                        assertEqual "to" True (requestsEqual next request)
    ]

appendEvent :: IORef [(Text, Text)] -> Text -> Text -> Playwright ()
appendEvent ref url value =
  modifyIORef' ref (<> [(url, value)])

eventsFor :: Text -> [(Text, Text)] -> [Text]
eventsFor wanted recorded =
  [value | (url, value) <- recorded, url == wanted]

redirectApp :: Application
redirectApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    ["foo.html"] ->
      respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    _ ->
      respond (responseLBS status404 [] "")

failedCssApp :: ByteString -> Application
failedCssApp html request respond =
  case pathInfo request of
    ["one-style.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] (LBS.fromStrict html))
    ["one-style.css"] ->
      respond (responseRaw (\_ _ -> pure ()) (responseLBS status500 [] ""))
    _ ->
      respond (responseLBS status404 [] "")

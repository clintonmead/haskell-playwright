module Playwright.PageRequestContinueSpec
  ( tests
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  , isJust
  )
import Data.Text
  ( isSuffixOf
  , pack
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( ContinueOptions (continueHeaders, continueMethod, continuePostData, continueUrl)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , HttpMethod (HttpMethod)
  , UrlGlob (UrlGlob)
  , continueRoute
  , continueRouteWith
  , defaultContinueOptions
  , defaultGotoOptions
  , evaluateText
  , goto
  , requestHeaders
  , responseOk
  , responseUrl
  , route
  , routeRequest
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( assertBool
  , withLocalApp
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
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
    "page-request-continue"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          route page (UrlGlob "**/*") continueRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should amend HTTP headers" $
        withApplication (pure echoFooHeader) $ \port ->
          withChromiumPage $ \page -> do
            route page (UrlGlob "**/*") $ \handled -> do
              headers <- requestHeaders =<< routeRequest handled
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueHeaders =
                      Just (headers <> [HttpHeader (HeaderName "foo") (HeaderValue "bar")])
                  }
            _ <-
              goto
                page
                (urlFromText ("http://127.0.0.1:" <> pack (show port) <> "/empty.html"))
                defaultGotoOptions
            assertEqual "foo" "bar" =<< evaluateText page "() => document.body.innerText"    , testCase "should amend method" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continueMethod = Just (HttpMethod "POST")}
            _ <- evaluateText page "() => fetch('/sleep.zzz').then(r => r.text())"
            seen <- readIORef requests
            assertEqual "method" (Just "POST") (methodOf "/sleep.zzz" seen)
    , testCase "should amend method on main request" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            route page (UrlGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continueMethod = Just (HttpMethod "POST")}
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- readIORef requests
            assertEqual "method" (Just "POST") (methodOf "/empty.html" seen)
    , testCase "should override request url" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            route page (UrlGlob "**/foo") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continueUrl = Just (urlFromText (prefix <> "/global-var.html"))}
            response <- goto page (urlFromText (prefix <> "/foo")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                finalUrl <- responseUrl value
                assertBool "response url" ("/global-var.html" `isSuffixOf` urlToText finalUrl)
            seen <- readIORef requests
            assertEqual "server saw the overridden path" True (isJust (methodOf "/global-var.html" seen))
    , testCase "should override method along with url" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            route page (UrlGlob "**/foo") $ \handled ->
              continueRouteWith
                handled
                defaultContinueOptions
                  { continueUrl = Just (urlFromText (prefix <> "/empty.html"))
                  , continueMethod = Just (HttpMethod "POST")
                  }
            _ <- goto page (urlFromText (prefix <> "/foo")) defaultGotoOptions
            seen <- readIORef requests
            assertEqual "method" (Just "POST") (methodOf "/empty.html" seen)
    , testCase "should amend post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just "doggo"}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just "doggo") (bodyOf "/sleep.zzz" seen)
    , testCase "should amend utf8 post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            route page (UrlGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just (encodeUtf8 "你好")}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just (encodeUtf8 "你好")) (bodyOf "/sleep.zzz" seen)
    , testCase "should amend longer post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let longBody = BS8.replicate 7500 'a'
            route page (UrlGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just longBody}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just longBody) (bodyOf "/sleep.zzz" seen)
    , testCase "should amend binary post data" $ do
        requests <- newIORef []
        withChromiumPage $ \page ->
          withLocalApp (recordingApp requests) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let binary = BS.pack [0, 1, 2, 127, 128, 255]
            route page (UrlGlob "**/*") $ \handled ->
              continueRouteWith handled defaultContinueOptions {continuePostData = Just binary}
            _ <- evaluateText page "() => fetch('/sleep.zzz', { method: 'POST', body: 'birdy' }).then(r => r.text())"
            seen <- readIORef requests
            assertEqual "body" (Just binary) (bodyOf "/sleep.zzz" seen)

    ]

echoFooHeader :: Application
echoFooHeader req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, BS8.pack "text/plain")]
        (LBS8.fromStrict (fromMaybe BS8.empty (lookup "foo" (Wai.requestHeaders req))))
    )

-- | One request the recording server saw: method, path, and body.
data Recorded = Recorded
  { recordedMethod :: BS8.ByteString
  , recordedPath :: BS8.ByteString
  , recordedBody :: BS8.ByteString
  }

-- | Records every request's method, path, and body, and answers with an
-- empty page.
recordingApp :: IORef [Recorded] -> Application
recordingApp requests request respond = do
  body <- Wai.strictRequestBody request
  modifyIORef' requests
    ( Recorded
        (Wai.requestMethod request)
        (Wai.rawPathInfo request)
        (LBS8.toStrict body)
        :
    )
  respond (responseLBS status200 [(hContentType, "text/html")] "")

-- | The method of the last request to the path.
methodOf :: BS8.ByteString -> [Recorded] -> Maybe BS8.ByteString
methodOf path requests =
  recordedMethod <$> lastFor path requests

-- | The body of the last request to the path.
bodyOf :: BS8.ByteString -> [Recorded] -> Maybe BS8.ByteString
bodyOf path requests =
  recordedBody <$> lastFor path requests

-- | The most recent request to the path. The list is newest first.
lastFor :: BS8.ByteString -> [Recorded] -> Maybe Recorded
lastFor path requests =
  case filter ((== path) . recordedPath) requests of
    latest : _ ->
      Just latest
    [] ->
      Nothing

module Playwright.PageNetworkRequestSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Control.Monad
  ( when
  )
import Data.Aeson
  ( object
  , (.=)
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  , isInfixOf
  , toLower
  )
import Network.HTTP.Types
  ( status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import Playwright
  ( HeaderName (headerNameToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HeaderValue (headerValueToText)
  , Playwright
  , Request
  , UrlGlob (UrlGlob)
  , click
  , defaultGotoOptions
  , evaluate
  , framesEqual
  , goto
  , mainFrame
  , onRequest
  , requestFrame
  , requestHeaders
  , requestPostData
  , requestPostDataJSON
  , requestUrl
  , responseRequest
  , setContent
  , urlFromText
  , urlToText
  , waitForRequest
  )
import Playwright.Harness
  ( locate
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
  )
import UnliftIO.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
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
    "page-network-request"
    [ testCase "should work for main frame navigation request" $
        withAssets $ \prefix page -> do
          withAsync (waitForRequest page (UrlGlob (prefix <> "/empty.html"))) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            request <- wait handle
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual ->
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
    , testCase "should work for fetch requests" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForRequest page (UrlGlob (prefix <> "/digits/1.png"))) $ \handle -> do
            _ <- evaluate page "() => fetch('/digits/1.png')"
            request <- wait handle
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual ->
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
    , testCase "should return headers" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              headers <- requestHeaders request
              let userAgent =
                    fmap (headerValueToText . httpHeaderValue)
                      ( filter
                          ( \header ->
                              toLower (headerNameToText (httpHeaderName header)) == "user-agent"
                          )
                          headers
                      )
              case userAgent of
                [agent] ->
                  assertBool "Chrome" ("Chrome" `isInfixOf` agent)
                _ ->
                  assertFailure "missing user-agent"
    , testCase "should expose request url after navigation" $
        withAssets $ \prefix page -> do
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value -> do
              request <- responseRequest value
              url <- requestUrl request
              assertEqual "url" (prefix <> "/empty.html") (urlToText url)
    , testCase "should return postData" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            _ <- evaluate page "() => fetch('./post', { method: 'POST', body: JSON.stringify({ foo: 'bar' }) })"
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual "postData" (Just (BS8.pack "{\"foo\":\"bar\"}")) =<< requestPostData request
    , testCase "should get undefined with postData() when there is no post data" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            case response of
              Nothing ->
                assertFailure "missing response"
              Just value -> do
                request <- responseRequest value
                assertEqual "postData" Nothing =<< requestPostData request
    , testCase "should parse the json post data" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            _ <- evaluate page "() => fetch('./post', { method: 'POST', body: JSON.stringify({ foo: 'bar' }) })"
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual "json" (Just (object ["foo" .= ("bar" :: Text)])) =<< requestPostDataJSON request
    , testCase "should parse the data if content-type is application/x-www-form-urlencoded" $
        withChromiumPage $ \page ->
          withLocalApp emptyApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            seen <- newIORef (Nothing :: Maybe Request)
            onRequest page (recordPost prefix seen)
            setContent
              page
              "<form method='POST' action='/post'><input type='text' name='foo' value='bar'><input type='number' name='baz' value='123'><input type='submit'></form>"
            click (locate page "input[type=submit]")
            readIORef seen >>= \found -> case found of
              Nothing ->
                assertFailure "missing post request"
              Just request ->
                assertEqual
                  "json"
                  (Just (object ["foo" .= ("bar" :: Text), "baz" .= ("123" :: Text)]))
                  =<< requestPostDataJSON request
    ]

-- | Records the request to the local /post path, for the post-data cases.
recordPost :: Text -> IORef (Maybe Request) -> Request -> Playwright ()
recordPost prefix seen request = do
  url <- urlToText <$> requestUrl request
  when (url == prefix <> "/post") (writeIORef seen (Just request))

-- | Answers every path with an empty 200, so the post-data cases get a response.
emptyApp :: Application
emptyApp _ respond =
  respond (responseLBS status200 [] LBS8.empty)

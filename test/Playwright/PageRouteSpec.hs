module Playwright.PageRouteSpec
  ( tests
  ) where

import Data.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Maybe
  ( isJust
  , isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , toLower
  )
import Playwright
  ( HeaderName (HeaderName, headerNameToText)
  , HeaderValue (HeaderValue, headerValueToText)
  , HttpHeader (HttpHeader, httpHeaderName, httpHeaderValue)
  , HttpMethod (httpMethodToText)
  , ResourceType (resourceTypeToText)
  , UrlGlob (UrlGlob)
  , continueRoute
  , currentUrl
  , defaultGotoOptions
  , framesEqual
  , goto
  , mainFrame
  , requestFrame
  , requestHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestResourceType
  , requestUrl
  , responseOk
  , route
  , routeRequest
  , setExtraHTTPHeaders
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-route"
    [ testCase "should intercept" $
        withAssets $ \prefix page -> do
          captured <- newIORef False
          route page (UrlGlob "**/empty.html") $ \handled -> do
            request <- routeRequest handled
            url <- requestUrl request
            assertBool "empty.html" ("empty.html" `isInfixOf` urlToText url)
            headers <- requestHeaders request
            assertBool "user-agent" (isJust (headerValue "user-agent" headers))
            method <- requestMethod request
            assertEqual "method" "GET" (httpMethodToText method)
            postData <- requestPostData request
            assertBool "postData" (isNothing postData)
            navigation <- requestIsNavigation request
            assertEqual "navigation" True navigation
            resourceType <- requestResourceType request
            assertEqual "resource" "document" (resourceTypeToText resourceType)
            frame <- requestFrame request
            case frame of
              Nothing ->
                assertFailure "expected frame"
              Just actual -> do
                assertEqual "main frame" True (framesEqual actual (mainFrame page))
                blank <- currentUrl actual
                assertEqual "blank" "about:blank" (urlToText blank)
            writeIORef captured True
            continueRoute handled
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
          assertEqual "intercepted" True =<< readIORef captured
    , testCase "should show custom HTTP headers" $
        withAssets $ \prefix page -> do
          setExtraHTTPHeaders
            page
            [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
          route page (UrlGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            headers <- requestHeaders request
            assertEqual "foo" (Just "bar") (headerValue "foo" headers)
            continueRoute handled
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should contain referer header" $
        withAssets $ \prefix page -> do
          captured <- newIORef []
          route page (UrlGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            modifyIORef' captured (request :)
            continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          requests <- reverse <$> readIORef captured
          case requests of
            (_ : css : _) -> do
              url <- requestUrl css
              assertBool "css" ("/one-style.css" `isInfixOf` urlToText url)
              headers <- requestHeaders css
              case headerValue "referer" headers of
                Nothing ->
                  assertFailure "missing referer"
                Just referer ->
                  assertBool "html" ("/one-style.html" `isInfixOf` referer)
            _ ->
              assertFailure "expected stylesheet request"
    ]

headerValue :: Text -> [HttpHeader] -> Maybe Text
headerValue name headers =
  case fmap (headerValueToText . httpHeaderValue)
    ( filter
        ( \header ->
            toLower (headerNameToText (httpHeaderName header)) == name
        )
        headers
    ) of
    [value] ->
      Just value
    _ ->
      Nothing

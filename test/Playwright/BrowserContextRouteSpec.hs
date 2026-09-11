module Playwright.BrowserContextRouteSpec
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
  ( BrowserContext
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody)
  , HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , HttpMethod (httpMethodToText)
  , Page
  , ResourceType (resourceTypeToText)
  , RouteOptions (routeTimes)
  , RouteTimes (RouteTimes)
  , UrlGlob (UrlGlob)
  , contextRoute
  , contextRouteWith
  , continueRoute
  , currentUrl
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , defaultRouteOptions
  , fallbackRoute
  , framesEqual
  , fulfillRoute
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
  , responseText
  , route
  , routeRequest
  , unrouteContext
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( withAssetsContextWith
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
    "browsercontext-route"
    [ testCase "should intercept" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          captured <- newIORef False
          contextRoute ctx (UrlGlob "**/empty.html") $ \handled -> do
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
    , testCase "should unroute" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          intercepted <- newIORef ([] :: [Int])
          contextRoute ctx (UrlGlob "**/*") $ \handled -> do
            modifyIORef' intercepted (1 :)
            fallbackRoute handled
          contextRoute ctx (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (2 :)
            fallbackRoute handled
          contextRoute ctx (UrlGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (3 :)
            fallbackRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "before" [3, 2, 1] . reverse =<< readIORef intercepted
          writeIORef intercepted []
          unrouteContext ctx (UrlGlob "**/empty.html")
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "after" [1] . reverse =<< readIORef intercepted
    , testCase "should yield to page.route" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          contextRoute ctx (UrlGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "context")}
          route page (UrlGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "page")}
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "page" =<< responseText value
    , testCase "should fall back to context.route" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          contextRoute ctx (UrlGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "context")}
          route page (UrlGlob "**/non-empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "page")}
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "context" =<< responseText value
    , testCase "should chain page fallback into context.route" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          contextRoute ctx (UrlGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "context")}
          route page (UrlGlob "**/empty.html") fallbackRoute
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "context" =<< responseText value
    , testCase "should support the times parameter with route matching" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          intercepted <- newIORef ([] :: [Int])
          contextRouteWith
            ctx
            (UrlGlob "**/empty.html")
            defaultRouteOptions {routeTimes = Just (RouteTimes 1)}
            $ \handled -> do
              modifyIORef' intercepted (1 :)
              continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "once" [1] . reverse =<< readIORef intercepted
    ]

withAssetsContextWithDefault :: (Text -> BrowserContext -> Page -> IO a) -> IO a
withAssetsContextWithDefault =
  withAssetsContextWith defaultNewContextOptions

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

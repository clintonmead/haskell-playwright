module Playwright.BrowserContextRouteSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
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
  , UrlMatch (UrlMatchGlob)
  , abortRoute
  , contextRoute
  , contextRouteWith
  , continueRoute
  , currentUrl
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , defaultRouteOptions
  , fallbackRoute
  , fallbackRouteWith
  , evaluateText
  , Response
  , ContinueOptions (continueUrl)
  , defaultContinueOptions
  , defaultFulfillOptions
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
  , unrouteContextAll
  , urlFromText
  , urlToText
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( withAssetsContextWith
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
    "browsercontext-route"
    [ testCase "should intercept" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          captured <- newIORef False
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
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
          contextRoute ctx (UrlMatchGlob "**/*") $ \handled -> do
            modifyIORef' intercepted (1 :)
            fallbackRoute handled
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (2 :)
            fallbackRoute handled
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (3 :)
            fallbackRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "before" [3, 2, 1] . reverse =<< readIORef intercepted
          writeIORef intercepted []
          unrouteContext ctx (UrlMatchGlob "**/empty.html")
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "after" [1] . reverse =<< readIORef intercepted
    , testCase "should yield to page.route" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "context")}
          route page (UrlMatchGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "page")}
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "page" =<< responseText value
    , testCase "should fall back to context.route" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "context")}
          route page (UrlMatchGlob "**/non-empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "page")}
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "body" "context" =<< responseText value
    , testCase "should chain page fallback into context.route" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "context")}
          route page (UrlMatchGlob "**/empty.html") fallbackRoute
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
            (UrlMatchGlob "**/empty.html")
            defaultRouteOptions {routeTimes = Just (RouteTimes 1)}
            $ \handled -> do
              modifyIORef' intercepted (1 :)
              continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "once" [1] . reverse =<< readIORef intercepted
    , testCase "context.unrouteAll removes all handlers" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          contextRoute ctx (UrlMatchGlob "**/*") abortRoute
          contextRoute ctx (UrlMatchGlob "**/empty.html") abortRoute
          unrouteContextAll ctx
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
    , testCase "should chain fallback" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          intercepted <- newIORef []
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (<> [1 :: Int])
            fallbackRoute handled
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (<> [2])
            fallbackRoute handled
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (<> [3])
            fallbackRoute handled
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "order" [3, 2, 1] =<< readIORef intercepted
    , testCase "should chain fallback w/ dynamic URL" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          intercepted <- newIORef []
          contextRoute ctx (UrlMatchGlob "**/bar") $ \handled -> do
            modifyIORef' intercepted (<> [1 :: Int])
            fallbackRouteWith handled defaultContinueOptions {continueUrl = Just (urlFromText (prefix <> "/empty.html"))}
          contextRoute ctx (UrlMatchGlob "**/foo") $ \handled -> do
            modifyIORef' intercepted (<> [2])
            fallbackRouteWith handled defaultContinueOptions {continueUrl = Just (urlFromText "http://localhost/bar")}
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled -> do
            modifyIORef' intercepted (<> [3])
            fallbackRouteWith handled defaultContinueOptions {continueUrl = Just (urlFromText "http://localhost/foo")}
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "order" [3, 2, 1] =<< readIORef intercepted
    , testCase "should not chain fulfill" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          failed <- newIORef False
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \_ ->
            writeIORef failed True
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled ->
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "fulfilled")}
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled ->
            fallbackRoute handled
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just _ ->
              assertEqual "body" "fulfilled" =<< evaluateText page "() => document.body.innerText"
          assertEqual "failed" False =<< readIORef failed
    , testCase "should not chain abort" $
        withAssetsContextWithDefault $ \prefix ctx page -> do
          failed <- newIORef False
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \_ ->
            writeIORef failed True
          contextRoute ctx (UrlMatchGlob "**/empty.html") abortRoute
          contextRoute ctx (UrlMatchGlob "**/empty.html") $ \handled ->
            fallbackRoute handled
          result <- try (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) :: Playwright (Either PlaywrightError (Maybe Response))
          case result of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected the navigation to be aborted"
          assertEqual "failed" False =<< readIORef failed
    ]

withAssetsContextWithDefault :: (Text -> BrowserContext -> Page -> Playwright a) -> IO a
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

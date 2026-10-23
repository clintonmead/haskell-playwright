module Playwright.InterceptionSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Maybe
  ( fromMaybe
  , isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , replace
  , splitOn
  , toLower
  )
import Network.HTTP.Types
  ( hLocation
  , status302
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
  ( ConsoleMessage (consoleMessageText)
  , ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillStatus)
  , HttpStatus (HttpStatus)
  , Page
  , Request
  , UrlMatch (UrlMatchGlob, UrlMatchRegex)
  , WaitForState (WaitForVisible)
  , continueRoute
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateText
  , evaluateTextWith
  , evaluateWith
  , framesEqual
  , fulfillRoute
  , goBack
  , goto
  , HeaderName (headerNameToText)
  , HttpHeader (httpHeaderName)
  , HttpMethod (httpMethodToText)
  , ResourceType (resourceTypeToText)
  , currentUrl
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
  , textArgument
  , textContent
  , urlFromText
  , urlToText
  , waitFor
  , waitForConsole
  , waitForWorker
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( locate
  , testAssetsDir
  , withAssets
  , withChromiumPage
  , assertProtocolContains
  , assertBool
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
  localOption (mkTimeout 30000000) $
    testGroup
      "interception"
      [ testCase "should work with navigation" $
        withRedirectAssets $ \prefix page -> do
          captured <- newIORef []
          route page (UrlMatchGlob "**/*") $ \handled -> do
            request <- routeRequest handled
            url <- requestUrl request
            let
              name = lastPathSegment (urlToText url)
            modifyIORef' captured ((name, request) :)
            continueRoute handled
          _ <- goto page (urlFromText (prefix <> "/rrredirect")) defaultGotoOptions
          requests <- readIORef captured
          assertNav requests "rrredirect" True
          assertNav requests "frame.html" True
          assertNav requests "script.js" False
          assertNav requests "style.css" False
      , testCase "should intercept after a service worker" $
          withSwAssets $ \prefix page enablePassthroughRedirect -> do
          _ <-
            goto
              page
              (urlFromText (prefix <> "/serviceworkers/fetchdummy/sw.html"))
              defaultGotoOptions
          _ <- evaluate page "() => window['activationPromise']"
          swResponse <-
            evaluateText page "() => window['fetchDummy']('foo')"
          assertEqual "sw" "responseFromServiceWorker:foo" swResponse
          route page (UrlMatchGlob "**/foo") $ \handled -> do
            request <- routeRequest handled
            url <- requestUrl request
            let
              name = lastPathSegment (urlToText url)
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillStatus = Just (HttpStatus 200)
                , fulfillContentType = Just (ContentType "text/css")
                , fulfillBody = Just (FulfillText ("responseFromInterception:" <> name))
                }
          swResponse2 <- evaluateText page "() => window['fetchDummy']('foo')"
          assertEqual "still sw" "responseFromServiceWorker:foo" swResponse2
          nonIntercepted <- evaluateText page "() => window['fetchDummy']('passthrough')"
          assertEqual "passthrough" "FAILURE: Not Found" nonIntercepted
          enablePassthroughRedirect
          redirected <- evaluateText page "() => window['fetchDummy']('passthrough')"
          assertEqual "redirect" "{\"foo\": \"bar\"}\n" redirected
      , testCase "should intercept by glob" $
          withAssets $ \prefix page -> do
            let
              hostPrefix = replace "127.0.0.1" "localhost" prefix
            _ <- goto page (urlFromText (hostPrefix <> "/empty.html")) defaultGotoOptions
            route page (UrlMatchGlob "http://localhos**?*oo") $ \handled ->
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 200)
                  , fulfillBody = Just (FulfillText "intercepted")
                  }
            result <-
              evaluateTextWith
                page
                "url => fetch(url).then(r => r.text())"
                (textArgument (hostPrefix <> "/?foo"))
            assertEqual "glob" "intercepted" result
      , testCase "should intercept network activity from worker" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            url = prefix <> "/data_for_worker"
          route page (UrlMatchGlob url) $ \handled ->
            ignoreFulfill $
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 200)
                  , fulfillBody = Just (FulfillText "intercepted")
                  }
          withAsync (waitForConsole page) $ \pending -> do
            _ <-
              evaluateWith
                page
                "url => new Worker(URL.createObjectURL(new Blob([`\n\
                \  fetch(\"${url}\").then(response => response.text()).then(console.log);\n\
                \`], { type: 'application/javascript' })))"
                (textArgument url)
            msg <- wait pending
            assertEqual "worker" "intercepted" (consoleMessageText msg)
    , testCase "should intercept worker requests when enabled after worker creation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            url = prefix <> "/data_for_worker"
          withAsync (waitForWorker page) $ \workerPending -> do
            _ <-
              evaluateWith
                page
                "url => {\n\
                \  window.w = new Worker(URL.createObjectURL(new Blob([`\n\
                \    onmessage = function(e) {\n\
                \      fetch(\"${url}\").then(response => response.text()).then(console.log);\n\
                \    };\n\
                \  `], { type: 'application/javascript' })));\n\
                \}"
                (textArgument url)
            _ <- wait workerPending
            route page (UrlMatchGlob url) $ \handled ->
              ignoreFulfill $
                fulfillRoute
                  handled
                  defaultFulfillOptions
                    { fulfillStatus = Just (HttpStatus 200)
                    , fulfillBody = Just (FulfillText "intercepted")
                    }
            withAsync (waitForConsole page) $ \consolePending -> do
              _ <- evaluate page "() => window.w.postMessage('')"
              msg <- wait consolePending
              assertEqual "late worker" "intercepted" (consoleMessageText msg)
    , testCase "should intercept network activity from worker 2" $
        withAssets $ \prefix page -> do
          let
            url = prefix <> "/worker/worker.js"
          route page (UrlMatchGlob url) $ \handled ->
            ignoreFulfill $
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillStatus = Just (HttpStatus 200)
                  , fulfillBody = Just (FulfillText "console.log(\"intercepted\");")
                  , fulfillContentType = Just (ContentType "application/javascript")
                  }
          withAsync (waitForConsole page) $ \pending -> do
            _ <-
              goto
                page
                (urlFromText (prefix <> "/worker/worker.html"))
                defaultGotoOptions
            msg <- wait pending
            assertEqual "worker.js" "intercepted" (consoleMessageText msg)
    , testCase "should not break remote worker importScripts" $
        withAssets $ \prefix page -> do
          route page (UrlMatchGlob "**") continueRoute
          _ <-
            goto
              page
              (urlFromText (prefix <> "/worker/worker-http-import.html"))
              defaultGotoOptions
          waitFor (locate page "#status:has-text('finished')") WaitForVisible
    , testCase "should disable memory cache when intercepting" $
        withAssets $ \prefix page -> do
          intercepted <- newIORef (0 :: Int)
          route page (UrlMatchGlob "**/page.html") $ \handled -> do
            modifyIORef' intercepted (+ 1)
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillBody = Just (FulfillText "success")
                }
          _ <- goto page (urlFromText (prefix <> "/page.html")) defaultGotoOptions
          body <- textContent (locate page "body")
          assertEqual "body" True (maybe False ("success" `containedIn`) body)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          count1 <- readIORef intercepted
          assertEqual "first" 1 count1
          _ <- goBack page
          count2 <- readIORef intercepted
          assertEqual "back" 2 count2
    , testCase "should intercept every request matching a global regexp" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          intercepted <- newIORef (0 :: Int)
          route page (UrlMatchRegex "/intercept-me" "g") $ \handled -> do
            modifyIORef' intercepted (+ 1)
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "intercepted")}
          mapM_ (\_ -> assertEqual "body" "intercepted" =<< evaluateText page "() => fetch('/intercept-me', { cache: 'no-store' }).then(r => r.text())") [1 :: Int, 2, 3]
          assertEqual "count" 3 =<< readIORef intercepted
    , testCase "should work with regular expression passed from a different context" $
        withAssets $ \prefix page -> do
          intercepted <- newIORef False
          route page (UrlMatchRegex "empty\\.html" "") $ \handled -> do
            request <- routeRequest handled
            url <- requestUrl request
            assertBool "url" ("empty.html" `isInfixOf` urlToText url)
            headers <- requestHeaders request
            assertBool "user-agent" (any ((== "user-agent") . toLower . headerNameToText . httpHeaderName) headers)
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
            writeIORef intercepted True
            continueRoute handled
          response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          case response of
            Nothing ->
              assertFailure "missing response"
            Just value ->
              assertEqual "ok" True =<< responseOk value
          assertEqual "intercepted" True =<< readIORef intercepted
      , testCase "should throw on page.route with invalid glob" $
        withAssets $ \_ page ->
          assertProtocolContains "unmatched '{'" $
            route page (UrlMatchGlob "http://*/foo{") $ \handled ->
              continueRoute handled
      , testCase "should throw on unbalanced glob braces" $
        withAssets $ \_ page -> do
          let
            invalidGlobs :: [(Text, Text)]
            invalidGlobs =
              [ ("{foo", "unmatched '{'")
              , ("}foo", "unmatched '}'")
              , ("http://*/foo{", "unmatched '{'")
              , ("**/*.png?{", "unmatched '{'")
              , ("https://example.com/{a", "unmatched '{'")
              , ("{{foo}", "nested '{' is not supported")
              ]
          mapM_
            ( \(glob, message) ->
                assertProtocolContains message $
                  route page (UrlMatchGlob glob) continueRoute
            )
            invalidGlobs
          -- Escaped braces remain literal and must not throw.
          route page (UrlMatchGlob "\\{foo") continueRoute
          route page (UrlMatchGlob "foo\\}") continueRoute
    ]

containedIn :: Text -> Text -> Bool
containedIn needle haystack =
  needle `isInfixOf` haystack

lastPathSegment :: Text -> Text
lastPathSegment url =
  case splitOn "/" url of
    [] ->
      url
    parts ->
      fromMaybe url (safeLast parts)

safeLast :: [a] -> Maybe a
safeLast items =
  case reverse items of
    [] ->
      Nothing
    (x : _) ->
      Just x

assertNav :: [(Text, Request)] -> Text -> Bool -> Playwright ()
assertNav requests name expected =
  case lookup name requests of
    Nothing ->
      assertFailure ("missing request " <> show name)
    Just request -> do
      actual <- requestIsNavigation request
      assertEqual (show name) expected actual

ignoreFulfill :: Playwright () -> Playwright ()
ignoreFulfill action = do
  result <- try action
  case result of
    Left (_ :: PlaywrightError) ->
      pure ()
    Right () ->
      pure ()

withRedirectAssets :: (Text -> Page -> Playwright a) -> IO a
withRedirectAssets action = do
  assets <- testAssetsDir
  withApplication (pure (redirectThenStatic assets False)) $ \port ->
    withChromiumPage $ \page ->
      action ("http://127.0.0.1:" <> pack (show port)) page

withSwAssets :: (Text -> Page -> Playwright () -> Playwright a) -> IO a
withSwAssets action = do
  assets <- testAssetsDir
  passthroughRedirect <- newIORef False
  withApplication (pure (swApp assets passthroughRedirect)) $ \port ->
    withChromiumPage $ \page ->
      action
        ("http://127.0.0.1:" <> pack (show port))
        page
        (writeIORef passthroughRedirect True)

swApp :: FilePath -> IORef Bool -> Application
swApp dir passthroughRedirect request respond = do
  enabled <- readIORef passthroughRedirect
  redirectThenStatic dir enabled request respond

redirectThenStatic :: FilePath -> Bool -> Application
redirectThenStatic dir passthroughRedirect request respond =
  case pathInfo request of
    ["rrredirect"] ->
      respond (responseLBS status302 [(hLocation, "/frames/one-frame.html")] "")
    ["serviceworkers", "fetchdummy", "passthrough"]
      | passthroughRedirect ->
          respond (responseLBS status302 [(hLocation, "/simple.json")] "")
    _ ->
      staticApp (defaultFileServerSettings dir) request respond

module Playwright.ChromiumSpec
  ( tests
  ) where

import Control.Monad
  ( void
  , when
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.CaseInsensitive
  ( mk
  )
import Data.Maybe
  ( fromMaybe
  , isJust
  , isNothing
  )
import Data.Text
  ( Text
  , intercalate
  , isInfixOf
  , isSuffixOf
  , pack
  , replace
  , unpack
  )
import Network.HTTP.Types
  ( RequestHeaders
  , hAuthorization
  , hContentType
  , hLocation
  , status200
  , status302
  , status401
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Playwright
  ( BrowserContext
  , ConsoleMessage (consoleMessageArgs, consoleMessageText, consoleMessageType)
  , ConsoleType (ConsoleLog)
  , ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillStatus)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpCredentials (HttpCredentials, httpCredentialsOrigin, httpCredentialsPassword, httpCredentialsSend, httpCredentialsUsername)
  , HttpHeader (HttpHeader)
  , HttpPassword (HttpPassword)
  , HttpStatus (HttpStatus)
  , HttpUsername (HttpUsername)
  , LaunchOptions (launchOptionsArgs)
  , NewContextOptions (newContextHttpCredentials)
  , Playwright
  , Request
  , Response
  , UrlMatch (UrlMatchGlob)
  , Worker
  , closeContext
  , closePage
  , contextRoute
  , continueRoute
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultLaunchOptions
  , defaultNewContextOptions
  , Page
  , evaluate
  , evaluateOnWorker
  , evaluateOnWorkerNumber
  , evaluateOnWorkerText
  , evaluateText
  , fulfillRoute
  , goto
  , jsonValueText
  , newContext
  , newPage
  , onConsole
  , onContextClose
  , onContextRequest
  , onContextResponse
  , onServiceWorker
  , onWorkerClose
  , pageContext
  , requestFrame
  , requestResponse
  , requestServiceWorker
  , requestUrl
  , responseFromServiceWorker
  , responseUrl
  , route
  , routeRequest
  , serviceWorkers
  , setContextExtraHTTPHeaders
  , setOffline
  , urlFromText
  , urlToText
  , waitForConsole
  , waitForContextConsole
  , waitForServiceWorker
  , waitForWorkerConsole
  , workersEqual
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , testAssetsDir
  , withAssets
  , withAssetsPrefix
  , withChromiumBrowser
  , withChromiumContext
  , withChromiumContextWith
  , withLocalApp
  , withPersistentLaunchWith
  )
import Playwright.SerializedValue
  ( SerializedValue
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  , tryPutMVar
  )

tests :: TestTree
tests =
  testGroup
    "chromium"
    [ testCase "should create a worker from a service worker" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          withAsync (waitForServiceWorker ctx) $ \pending -> do
            _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
            worker <- wait pending
            assertEqual "scope" "[object ServiceWorkerGlobalScope]"
              =<< evaluateOnWorkerText worker "() => self.toString()"
    , testCase "should create a worker from service worker with noop routing" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          contextRoute ctx (UrlMatchGlob "**/*") continueRoute
          withAsync (waitForServiceWorker ctx) $ \pending -> do
            _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
            worker <- wait pending
            assertEqual "scope" "[object ServiceWorkerGlobalScope]"
              =<< evaluateOnWorkerText worker "() => self.toString()"
    , testCase "serviceWorkers() should return current workers" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
            cross = replace "127.0.0.1" "localhost" prefix
          worker1 <-
            withAsync (waitForServiceWorker ctx) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
              wait pending
          listed1 <- serviceWorkers ctx
          assertEqual "one" 1 (length listed1)
          worker2 <-
            withAsync (waitForServiceWorker ctx) $ \pending -> do
              _ <- goto page (urlFromText (cross <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
              wait pending
          listed2 <- serviceWorkers ctx
          assertEqual "two" 2 (length listed2)
          assertBool "contains first" (any (workersEqual worker1) listed2)
          assertBool "contains second" (any (workersEqual worker2) listed2)
    , testCase "should not create a worker from a shared worker" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          created <- newIORef False
          onServiceWorker (pageContext page) (\_ -> writeIORef created True)
          _ <- evaluate page "() => { new SharedWorker('data:text/javascript,console.log(\"hi\")'); }"
          assertEqual "no service worker" False =<< readIORef created
    , testCase "should pass args with spaces" $
        withPersistentLaunchWith
          defaultLaunchOptions {launchOptionsArgs = Just ["--user-agent=I am Foo"]}
          defaultNewContextOptions
          $ \_ctx page ->
            assertEqual "ua" "I am Foo" =<< evaluateText page "() => navigator.userAgent"
    , testCase "should close service worker together with the context" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            ctx <- newContext browser
            page <- newPage ctx
            worker <-
              withAsync (waitForServiceWorker ctx) $ \pending -> do
                _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
                wait pending
            messages <- newIORef ([] :: [Text])
            onContextClose ctx (modifyIORef' messages (<> ["context"]))
            onWorkerClose worker (modifyIORef' messages (<> ["worker"]))
            closeContext ctx
            assertEqual "order" "worker|context" . intercalate "|" =<< readIORef messages
    , testCase "serviceWorker(), and fromServiceWorker() work" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          htmlVar <- collectFirstRequest ctx (urlEndsWith "/sw.html")
          mainVar <- collectFirstRequest ctx (urlEndsWith "/sw.js")
          inWorkerVar <- collectFirstRequest ctx (urlEndsWith "/request-from-within-worker.txt")
          withAsync (waitForServiceWorker ctx) $ \swPending ->
            withAsync
              ( do
                  html <- takeMVarNamed "sw.html request" htmlVar
                  mainReq <- takeMVarNamed "sw.js request" mainVar
                  inWorker <- takeMVarNamed "request-from-within-worker.txt request" inWorkerVar
                  pure (html, mainReq, inWorker)
              )
              $ \reqsPending -> do
                _ <- goto page (urlFromText (prefix <> "/serviceworkers/fetch/sw.html")) defaultGotoOptions
                worker <- wait swPending
                (html, mainReq, inWorker) <- wait reqsPending
                assertBool "html frame" . isJust =<< requestFrame html
                assertBool "html has no worker" . isNothing =<< requestServiceWorker html
                htmlResponse <- requireResponse html
                assertEqual "html from sw" False =<< responseFromServiceWorker htmlResponse
                assertBool "main has no frame" . isNothing =<< requestFrame mainReq
                mainWorker <- requireWorker mainReq
                assertBool "main worker" (workersEqual mainWorker worker)
                -- The sw.js response is not reliably reported (see TODO), so only the request is checked.
                assertBool "in-worker has no frame" . isNothing =<< requestFrame inWorker
                inWorkerOwner <- requireWorker inWorker
                assertBool "in-worker owner" (workersEqual inWorkerOwner worker)
                inWorkerResponse <- requireResponse inWorker
                assertEqual "in-worker from sw" False =<< responseFromServiceWorker inWorkerResponse
                _ <- evaluateNamed "activationPromise" page "() => window['activationPromise']"
                innerSWVar <- newEmptyMVar
                innerPageVar <- newEmptyMVar
                onContextRequest ctx $ \request -> do
                  suffix <- urlEndsWith "/inner.txt" request
                  sw <- requestServiceWorker request
                  when (suffix && isJust sw) (void (tryPutMVar innerSWVar request))
                  when (suffix && isNothing sw) (void (tryPutMVar innerPageVar request))
                withAsync (takeMVarTimed innerSWVar) $ \innerSwPending ->
                  withAsync (takeMVarTimed innerPageVar) $ \innerPagePending -> do
                    _ <- evaluateNamed "fetch /inner.txt" page "() => fetch('/inner.txt')"
                    innerSW <- wait innerSwPending
                    innerPage <- wait innerPagePending
                    assertBool "page fetch has no worker" . isNothing =<< requestServiceWorker innerPage
                    innerPageResponse <- requireResponse innerPage
                    assertEqual "page from sw" True =<< responseFromServiceWorker innerPageResponse
                    innerSWOwner <- requireWorker innerSW
                    assertBool "sw fetch owner" (workersEqual innerSWOwner worker)
                    innerSWResponse <- requireResponse innerSW
                    assertEqual "sw fetch from sw" False =<< responseFromServiceWorker innerSWResponse
    , testCase "should intercept service worker requests (main and within)" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          contextRoute ctx (UrlMatchGlob "**/request-from-within-worker") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "application/json")
                , fulfillStatus = Just (HttpStatus 200)
                , fulfillBody = Just (FulfillText "\"intercepted!\"")
                }
          contextRoute ctx (UrlMatchGlob "**/sw.js") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/javascript")
                , fulfillStatus = Just (HttpStatus 200)
                , fulfillBody =
                    Just
                      ( FulfillText
                          "self.contentPromise = new Promise(res => fetch('/request-from-within-worker').then(r => r.json()).then(res));"
                      )
                }
          gotWithin <- newEmptyMVar
          gotSwJsRequest <- newEmptyMVar
          gotSwJsResponse <- newEmptyMVar
          onContextRequest ctx $ \request -> do
            suffix <- urlEndsWith "sw.js" request
            when suffix (void (tryPutMVar gotSwJsRequest request))
          onContextResponse ctx $ \response -> do
            url <- urlToText <$> responseUrl response
            fromSW <- responseFromServiceWorker response
            when ("/request-from-within-worker" `isSuffixOf` url) (void (tryPutMVar gotWithin response))
            when ("sw.js" `isSuffixOf` url && not fromSW) (void (tryPutMVar gotSwJsResponse response))
          withAsync (waitForServiceWorker ctx) $ \swPending -> do
            _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
            worker <- wait swPending
            _ <- takeMVarNamed "sw.js request" gotSwJsRequest
            _ <- takeMVarNamed "within response" gotWithin
            intercepted <-
              evaluateOnWorkerText
                worker
                "() => Promise.race([self['contentPromise'], new Promise((_, reject) => setTimeout(() => reject(new Error('contentPromise timeout')), 5000))])"
            assertEqual "intercepted" "intercepted!" intercepted
    , testCase "should intercept service worker importScripts" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          contextRoute ctx (UrlMatchGlob "**/import.js") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/javascript")
                , fulfillStatus = Just (HttpStatus 200)
                , fulfillBody = Just (FulfillText "self.exportedValue = 47;")
                }
          contextRoute ctx (UrlMatchGlob "**/sw.js") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/javascript")
                , fulfillStatus = Just (HttpStatus 200)
                , fulfillBody =
                    Just
                      ( FulfillText
                          "importScripts('/import.js');\nself.importedValue = self.exportedValue;"
                      )
                }
          importVar <- collectFirstResponse ctx (responseUrlEndsWith "/import.js")
          withAsync (waitForServiceWorker ctx) $ \swPending ->
            withAsync (takeMVarNamed "import.js response" importVar) $ \importPending -> do
              _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
              worker <- wait swPending
              _ <- wait importPending
              assertEqual "imported" (47 :: Double)
                =<< evaluateOnWorkerNumber worker "() => self['importedValue']"
    , testCase "should intercept only serviceworker request, not page" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          contextRoute ctx (UrlMatchGlob "**/data.json") $ \handled -> do
            request <- routeRequest handled
            sw <- requestServiceWorker request
            case sw of
              Just _ ->
                fulfillRoute
                  handled
                  defaultFulfillOptions
                    { fulfillContentType = Just (ContentType "text/plain")
                    , fulfillStatus = Just (HttpStatus 200)
                    , fulfillBody = Just (FulfillText "from sw")
                    }
              Nothing ->
                continueRoute handled
          worker <-
            withAsync (waitForServiceWorker ctx) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/serviceworkers/fetch/sw.html")) defaultGotoOptions
              wait pending
          _ <- evaluate page "() => window['activationPromise']"
          response <- evaluateText page "() => fetch('/data.json').then(r => r.text())"
          intercepted <- evaluateOnWorkerText worker "() => self['intercepted'][0]"
          assertBool "intercepted data.json" ("/data.json" `isSuffixOf` intercepted)
          assertEqual "from sw" "from sw" response
    , testCase "setOffline" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          worker <-
            withAsync (waitForServiceWorker ctx) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/serviceworkers/fetch/sw.html")) defaultGotoOptions
              wait pending
          _ <- evaluate page "() => window['activationPromise']"
          setOffline ctx True
          innerVar <- collectFirstRequest ctx $ \request -> do
            suffix <- urlEndsWith "/inner.txt" request
            sw <- requestServiceWorker request
            pure (suffix && isJust sw)
          err <-
            evaluateOnWorkerText
              worker
              "() => fetch('/inner.txt').catch(e => 'REJECTED: ' + e)"
          _ <- takeMVarNamed "offline inner.txt request" innerVar
          assertBool "rejected" ("REJECTED" `isInfixOf` err)
          assertBool "failed to fetch" ("Failed to fetch" `isInfixOf` err)
    , testCase "setExtraHTTPHeaders" $ do
        captured <- newEmptyMVar
        assets <- testAssetsDir
        withChromiumContext $ \ctx page ->
          withLocalApp (innerHeaderApp assets captured) $ \prefix -> do
            worker <-
              withAsync (waitForServiceWorker ctx) $ \pending -> do
                _ <- goto page (urlFromText (prefix <> "/serviceworkers/fetch/sw.html")) defaultGotoOptions
                wait pending
            _ <- evaluate page "() => window['activationPromise']"
            setContextExtraHTTPHeaders
              ctx
              [HttpHeader (HeaderName "x-custom-header") (HeaderValue "custom!")]
            withAsync (takeMVarTimed captured) $ \pending -> do
              _ <- evaluateOnWorker worker "() => fetch('/inner.txt')"
              headers <- wait pending
              assertEqual
                "custom header"
                (Just (BS8.pack "custom!"))
                (lookup (mk (BS8.pack "x-custom-header")) headers)
    , testCase "should emit console messages from service worker" $
        withAssets $ \prefix page -> do
          let
            ctx = pageContext page
          emptyPage <- newPage ctx
          _ <- goto emptyPage (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          emptyMessages <- newIORef ([] :: [ConsoleMessage])
          onConsole emptyPage (\message -> modifyIORef' emptyMessages (<> [message]))
          worker <-
            withAsync (waitForServiceWorker ctx) $ \pending -> do
              _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
              wait pending
          withAsync (waitForWorkerConsole worker) $ \workerPending ->
            withAsync (waitForConsole page) $ \pagePending -> do
              _ <-
                evaluateOnWorker
                  worker
                  "() => console.log('hello from service worker', { i: 'am', am: 1, complex: { yup: true } })"
              workerMessage <- wait workerPending
              pageMessage <- wait pagePending
              assertEqual "type" ConsoleLog (consoleMessageType workerMessage)
              assertEqual "same text" (consoleMessageText workerMessage) (consoleMessageText pageMessage)
              assertBool "text" ("hello from service worker" `isInfixOf` consoleMessageText workerMessage)
              assertEqual "empty page quiet" 0 . length =<< readIORef emptyMessages
              closePage page
              assertEqual "args" 2 (length (consoleMessageArgs workerMessage))
              case consoleMessageArgs workerMessage of
                firstArg : _ ->
                  assertEqual "arg0" "hello from service worker" =<< jsonValueText firstArg
                _ ->
                  assertFailure "expected console args"
    , testCase "should capture console.log from ServiceWorker start" $ do
        assets <- testAssetsDir
        withChromiumContext $ \ctx page ->
          withLocalApp (swStartLogApp assets) $ \prefix ->
            withAsync (waitForServiceWorker ctx) $ \swPending ->
              withAsync (waitForContextConsole ctx) $ \consolePending -> do
                _ <- goto page (urlFromText (prefix <> "/serviceworkers/empty/sw.html")) defaultGotoOptions
                _ <- wait swPending
                message <- wait consolePending
                assertEqual "text" "Hello from the first line of sw.js" (consoleMessageText message)
                assertEqual "type" ConsoleLog (consoleMessageType message)
    , testCase "httpCredentials" $ do
        assets <- testAssetsDir
        withChromiumContextWith
          defaultNewContextOptions
            { newContextHttpCredentials =
                Just
                  HttpCredentials
                    { httpCredentialsUsername = HttpUsername "user"
                    , httpCredentialsPassword = HttpPassword "pass"
                    , httpCredentialsOrigin = Nothing
                    , httpCredentialsSend = Nothing
                    }
            }
          $ \ctx page ->
            withLocalApp (fetchAuthApp assets) $ \prefix -> do
              worker <-
                withAsync (waitForServiceWorker ctx) $ \pending -> do
                  _ <- goto page (urlFromText (prefix <> "/serviceworkers/fetch/sw.html")) defaultGotoOptions
                  wait pending
              _ <- evaluate page "() => window['activationPromise']"
              assertEqual "status" (200 :: Double)
                =<< evaluateOnWorkerNumber worker "() => fetch('/empty.html').then(r => r.status)"
    , testCase "Page.route should work with intervention headers" $ do
        captured <- newEmptyMVar
        withChromiumContext $ \_ctx page ->
          withLocalApp (interventionApp captured) $ \prefix -> do
            route page (UrlMatchGlob "*") continueRoute
            _ <- goto page (urlFromText (prefix <> "/intervention")) defaultGotoOptions
            headers <- takeMVarTimed captured
            let
              header = lookup (mk (BS8.pack "intervention")) headers
            assertBool "intervention header" $
              maybe False (BS8.isInfixOf (BS8.pack "feature/5718547946799104")) header
    ]

urlEndsWith :: Text -> Request -> Playwright Bool
urlEndsWith suffix request =
  (suffix `isSuffixOf`) . urlToText <$> requestUrl request

evaluateNamed :: String -> Page -> Text -> Playwright SerializedValue
evaluateNamed label page expression = do
  outcome <- race (threadDelay 30000000) (evaluate page expression)
  case outcome of
    Left () ->
      assertFailure ("evaluate stalled: " <> label)
    Right value ->
      pure value

takeMVarNamed :: String -> MVar a -> Playwright a
takeMVarNamed label var = do
  outcome <- race (threadDelay 30000000) (takeMVar var)
  case outcome of
    Left () ->
      assertFailure ("timed out waiting for " <> label)
    Right value ->
      pure value

-- | Keeps the first context request that matches. Register it before the
-- action that makes the request: a waiter started after the request fired
-- misses it.
collectFirstRequest :: BrowserContext -> (Request -> Playwright Bool) -> Playwright (MVar Request)
collectFirstRequest ctx matches = do
  found <- newEmptyMVar
  onContextRequest ctx $ \request -> do
    matched <- matches request
    when matched (void (tryPutMVar found request))
  pure found

-- | Keeps the first context response that matches. See 'collectFirstRequest'.
collectFirstResponse :: BrowserContext -> (Response -> Playwright Bool) -> Playwright (MVar Response)
collectFirstResponse ctx matches = do
  found <- newEmptyMVar
  onContextResponse ctx $ \response -> do
    matched <- matches response
    when matched (void (tryPutMVar found response))
  pure found

takeMVarTimed :: MVar a -> Playwright a
takeMVarTimed var = do
  outcome <- race (threadDelay 30000000) (takeMVar var)
  case outcome of
    Left () ->
      assertFailure "timed out waiting for event"
    Right value ->
      pure value

responseUrlEndsWith :: Text -> Response -> Playwright Bool
responseUrlEndsWith suffix response =
  (suffix `isSuffixOf`) . urlToText <$> responseUrl response

requireResponse :: Request -> Playwright Response
requireResponse request = do
  outcome <- race (threadDelay 30000000) (requestResponse request)
  pending <- case outcome of
    Left () -> do
      url <- requestUrl request
      assertFailure ("timed out waiting for request.response() of " <> show (urlToText url))
    Right found ->
      pure found
  case pending of
    Nothing ->
      assertFailure "expected response"
    Just response ->
      pure response

requireWorker :: Request -> Playwright Worker
requireWorker request = do
  pending <- requestServiceWorker request
  case pending of
    Nothing ->
      assertFailure "expected service worker"
    Just worker ->
      pure worker

innerHeaderApp :: FilePath -> MVar RequestHeaders -> Application
innerHeaderApp assets captured request respond =
  case pathInfo request of
    ["inner.txt"] -> do
      putMVar captured (requestHeaders request)
      respond (responseLBS status200 [] "ok")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

swStartLogApp :: FilePath -> Application
swStartLogApp assets request respond =
  case pathInfo request of
    ["serviceworkers", "empty", "sw.js"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/javascript")]
            (LBS8.pack "console.log('Hello from the first line of sw.js');")
        )
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

fetchAuthApp :: FilePath -> Application
fetchAuthApp assets request respond =
  let
    needsAuth =
      case pathInfo request of
        ["serviceworkers", "fetch", "sw.html"] ->
          True
        ["empty.html"] ->
          True
        _ ->
          False
    authorized =
      lookup hAuthorization (requestHeaders request) == Just (BS8.pack "Basic dXNlcjpwYXNz")
  in
    if needsAuth && not authorized
      then
        respond
          ( responseLBS
              status401
              [("WWW-Authenticate", BS8.pack "Basic realm=\"test\"")]
              LBS8.empty
          )
      else
        if pathInfo request == ["empty.html"] && authorized
          then
            respond
              ( responseLBS
                  status200
                  [(hContentType, "text/html")]
                  (LBS8.pack "<html><title>ok</title></html>")
              )
          else
            staticApp (defaultFileServerSettings assets) request respond

interventionApp :: MVar RequestHeaders -> Application
interventionApp captured request respond =
  case pathInfo request of
    ["intervention"] -> do
      let
        host = fromMaybe (BS8.pack "127.0.0.1") (lookup (mk (BS8.pack "host")) (requestHeaders request))
        cross = "http://" <> unpack (replace "127.0.0.1" "localhost" (pack (BS8.unpack host)))
        html =
          LBS8.pack
            ( "<script>document.write('<script src=\""
                <> cross
                <> "/intervention.js\"></scr' + 'ipt>');</script>"
            )
      respond (responseLBS status200 [(hContentType, "text/html")] html)
    ["intervention.js"] ->
      respond (responseLBS status302 [(hLocation, "/redirect.js")] LBS8.empty)
    ["redirect.js"] -> do
      putMVar captured (requestHeaders request)
      respond
        ( responseLBS
            status200
            [(hContentType, "text/javascript")]
            (LBS8.pack "console.log(1);")
        )
    _ ->
      respond (responseLBS status404 [] LBS8.empty)

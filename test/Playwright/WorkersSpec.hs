module Playwright.WorkersSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( void
  , when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import Data.List
  ( sort
  )
import Data.Maybe
  ( isJust
  , listToMaybe
  )
import Data.Text
  ( Text
  , breakOn
  , isInfixOf
  , replace
  , toLower
  , unpack
  )
import Playwright
  ( ConsoleMessage (consoleMessageArgs, consoleMessageText, consoleMessageTimestamp)
  , ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType, fulfillStatus)
  , HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , HttpStatus (HttpStatus)
  , PageError (pageErrorMessage)
  , Page
  , Playwright
  , Request
  , PropertyName (PropertyName)
  , UrlMatch (UrlMatchGlob)
  , Worker
  , armConsole
  , armContextConsole
  , armRequest
  , armRequestFinished
  , armRequestWhere
  , armResponse
  , armWorkerConsole
  , browserVersion
  , contextBrowser
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateNumber
  , evaluateOnWorker
  , evaluateOnWorkerNumber
  , evaluateOnWorkerText
  , framesEqual
  , fulfillRoute
  , getProperty
  , goto
  , jsonValueText
  , onConsole
  , onWorkerClose
  , pageContext
  , requestAllHeaders
  , requestFrame
  , requestResponse
  , requestUrl
  , requestsEqual
  , responseAllHeaders
  , responseOk
  , responseRequest
  , responseText
  , route
  , setExtraHTTPHeaders
  , setOffline
  , url
  , urlFromText
  , urlToText
  , waitForPageError
  , waitForWorker
  , workerUrl
  , workers
  )
import Playwright.Harness
  ( attachFrame
  , withAssetPage
  , withAssets
  , withChromiumPage
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
    "workers"
    [ testCase "Page.workers" $
        withAssets $ \prefix page ->
          withAsync (waitForWorker page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/worker/worker.html")) defaultGotoOptions
            worker <- wait handle
            workerLink <- workerUrl worker
            assertBool "worker.js" ("worker.js" `isInfixOf` urlToText workerLink)
            assertEqual "function" "worker function result" =<< evaluateOnWorkerText worker "() => self['workerFunction']()"
            listed <- workers page
            assertEqual "one worker" 1 (length listed)
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "gone" 0 . length =<< workers page
    , testCase "should report console logs" $
        withAssetPage "empty.html" $ \page -> do
          takeMessage <- armConsole page
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1)'], { type: 'application/javascript' }))); }"
          message <- takeMessage
          assertEqual "text" "1" (consoleMessageText message)
          pageLink <- url page
          assertBool "page url" (not ("blob" `isInfixOf` urlToText pageLink))
    , testCase "should not report console logs from workers twice" $
        withAssetPage "empty.html" $ \page -> do
          messages <- newIORef []
          bothSeen <- newEmptyMVar
          onConsole page $ \message -> do
            modifyIORef' messages (consoleMessageText message :)
            when (consoleMessageText message == "2") (void (tryPutMVar bothSeen ()))
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1)'], { type: 'application/javascript' }))); new Worker(URL.createObjectURL(new Blob(['console.log(2)'], { type: 'application/javascript' }))); }"
          -- Bounded: a missing message must fail the test, not hang the run.
          outcome <- race (liftIO (threadDelay 30000000)) (takeMVar bothSeen)
          case outcome of
            Left () ->
              assertFailure "worker console messages never arrived"
            Right () -> do
              -- Give any duplicate delivery a chance to arrive before counting.
              liftIO (threadDelay 500000)
              assertEqual "texts" ["1", "2"] . sort =<< readIORef messages
    , testCase "should have timestamp on worker console messages" $
        withAssetPage "empty.html" $ \page -> do
          before <- evaluateNumber page "() => Date.now() - 1"
          takeMessage <- armConsole page
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(\"ts\")'], { type: 'application/javascript' }))); }"
          message <- takeMessage
          after <- evaluateNumber page "() => Date.now() + 1"
          assertBool "timestamp in range" (consoleMessageTimestamp message >= before && consoleMessageTimestamp message <= after)
    , testCase "should have JSHandles for console logs" $
        -- The default about:blank page gives the blob worker an opaque origin, so origin is "null".
        withChromiumPage $ \page -> do
          takeMessage <- armConsole page
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1,2,3,this)'], { type: 'application/javascript' }))); }"
          message <- takeMessage
          assertEqual "text" "1 2 3 DedicatedWorkerGlobalScope" (consoleMessageText message)
          case consoleMessageArgs message of
            [_, _, _, scope] -> do
              origin <- getProperty scope (PropertyName "origin")
              assertEqual "origin" "null" =<< jsonValueText origin
            _ ->
              assertFailure "expected four console arguments"
    , testCase "should evaluate" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1)'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          assertEqual "sum" (2 :: Double) =<< evaluateOnWorkerNumber worker "() => 1 + 1"
    , testCase "should clear upon navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1)'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          assertEqual "one worker" 1 . length =<< workers page
          destroyed <- newIORef False
          onWorkerClose worker (writeIORef destroyed True)
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          assertEqual "destroyed" True =<< readIORef destroyed
          assertEqual "gone" 0 . length =<< workers page
    , testCase "should clear upon cross-process navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['console.log(1)'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          assertEqual "one worker" 1 . length =<< workers page
          destroyed <- newIORef False
          onWorkerClose worker (writeIORef destroyed True)
          -- A different origin stands in for the other process, as in BrowserContextBasicSpec.
          let
            cross = replace "127.0.0.1" "localhost" prefix <> "/empty.html"
          _ <- goto page (urlFromText cross) defaultGotoOptions
          assertEqual "destroyed" True =<< readIORef destroyed
          assertEqual "gone" 0 . length =<< workers page
    , testCase "should attribute network activity for worker inside iframe to the iframe" $
        withAssets $ \prefix page -> do
          -- Attributing a worker's requests to its iframe needs chromium 149.
          browser <- contextBrowser (pageContext page)
          version <- browserVersion browser
          let major = read (unpack (fst (breakOn "." version))) :: Int
          if major < 149
            then liftIO (putStrLn ("skipped: needs chromium 149, found " <> unpack version))
            else do
              _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              withAsync (waitForWorker page) $ \handle -> do
                frame <- attachFrame page "frame1" (prefix <> "/worker/worker.html")
                worker <- wait handle
                let
                  link = prefix <> "/one-style.css"
                takeRequest <- armRequest page (UrlMatchGlob link)
                _ <- evaluateOnWorker worker ("() => fetch('" <> link <> "').then(response => response.text()).then(console.log)")
                request <- takeRequest
                assertEqual "request url" link . urlToText =<< requestUrl request
                assertBool "request frame" . maybe False (framesEqual frame) =<< requestFrame request
    , testCase "should emit created and destroyed events" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.__obj = new Worker(URL.createObjectURL(new Blob(['1'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          closed <- newEmptyMVar
          onWorkerClose worker (void (tryPutMVar closed ()))
          _ <- evaluate page "() => window.__obj.terminate()"
          -- Bounded: a close event that never arrives must fail the test, not hang the run.
          outcome <- race (liftIO (threadDelay 30000000)) (takeMVar closed)
          case outcome of
            Left () ->
              assertFailure "worker close event never arrived"
            Right () ->
              pure ()
    , testCase "should report errors" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob([\"setTimeout(() => { console.log('hey'); throw new Error('this is my error'); })\"], { type: 'application/javascript' }))); }"
          err <- waitForPageError page
          assertBool "message" ("this is my error" `isInfixOf` pageErrorMessage err)
    , testCase "should dispatch console messages when page has workers" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['1'], { type: 'application/javascript' }))); }"
          _ <- waitForWorker page
          takeMessage <- armConsole page
          _ <- evaluate page "() => console.log('page')"
          message <- takeMessage
          assertEqual "text" "page" (consoleMessageText message)
    , testCase "should report console event on the worker" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['1'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          takeWorkerMessage <- armWorkerConsole worker
          takePageMessage <- armConsole page
          takeContextMessage <- armContextConsole (pageContext page)
          _ <- evaluateOnWorkerText worker "() => { console.log('hey'); return 'ok'; }"
          workerMessage <- takeWorkerMessage
          pageMessage <- takePageMessage
          contextMessage <- takeContextMessage
          assertEqual "worker text" "hey" (consoleMessageText workerMessage)
          assertEqual "page text" "hey" (consoleMessageText pageMessage)
          assertEqual "context text" "hey" (consoleMessageText contextMessage)
    , testCase "should report console event on the worker when not listening on page or context" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { new Worker(URL.createObjectURL(new Blob(['1'], { type: 'application/javascript' }))); }"
          worker <- waitForWorker page
          takeWorkerMessage <- armWorkerConsole worker
          _ <- evaluateOnWorkerText worker "() => { console.log('hey'); return 'ok'; }"
          workerMessage <- takeWorkerMessage
          assertEqual "worker text" "hey" (consoleMessageText workerMessage)
    , testCase "should report network activity" $
        withAssets $ \prefix page ->
          withAsync (waitForWorker page) $ \handle -> do
            -- workerScriptLoaded is only reported from chromium 143.
            browser <- contextBrowser (pageContext page)
            version <- browserVersion browser
            let major = read (unpack (fst (breakOn "." version))) :: Int
            if major < 143
              then liftIO (putStrLn ("skipped: needs chromium 143, found " <> unpack version))
              else do
                _ <- goto page (urlFromText (prefix <> "/worker/worker.html")) defaultGotoOptions
                worker <- wait handle
                let
                  link = prefix <> "/one-style.css"
                takeRequest <- armRequest page (UrlMatchGlob link)
                takeResponse <- armResponse page (UrlMatchGlob link)
                _ <- evaluateOnWorker worker ("() => fetch('" <> link <> "').then(response => response.text()).then(console.log)")
                request <- takeRequest
                response <- takeResponse
                assertEqual "request url" link . urlToText =<< requestUrl request
                assertBool "response request" . requestsEqual request =<< responseRequest response
                assertBool "ok" =<< responseOk response
                assertBool "text" . isInfixOf "background-color: pink" =<< responseText response
    , testCase "should report network activity on worker creation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            link = prefix <> "/one-style.css"
          takeRequest <- armRequest page (UrlMatchGlob link)
          takeResponse <- armResponse page (UrlMatchGlob link)
          _ <-
            evaluate
              page
              ( "() => new Worker(URL.createObjectURL(new Blob(['fetch(\"" <> link <> "\").then(response => response.text()).then(console.log);'], { type: 'application/javascript' })))"
              )
          request <- takeRequest
          response <- takeResponse
          assertEqual "request url" link . urlToText =<< requestUrl request
          assertBool "response request" . requestsEqual request =<< responseRequest response
          assertBool "ok" =<< responseOk response
    , testCase "should report worker script as network request" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          takeRequest <-
            armRequestWhere page (\request -> isInfixOf "worker.js" . urlToText <$> requestUrl request)
          takeFinished <- armRequestFinished page
          _ <- evaluate page "() => (window.w = new Worker('/worker/worker.js'))"
          request1 <- takeRequest
          request2 <- awaitScriptFinished page takeFinished
          assertEqual "request url" (prefix <> "/worker/worker.js") . urlToText =<< requestUrl request1
          assertBool "same request" (requestsEqual request1 request2)
          response <- requestResponse request1
          case response of
            Nothing ->
              assertFailure "worker.js request has no response"
            Just found -> do
              text <- responseText found
              assertBool "script text" ("console.log('hello from the worker');" `isInfixOf` text)
    , testCase "should report and intercept network from nested worker" $
        withAssets $ \prefix page -> do
          -- workerScriptLoaded is only reported from chromium 143.
          browser <- contextBrowser (pageContext page)
          version <- browserVersion browser
          let major = read (unpack (fst (breakOn "." version))) :: Int
          if major < 143
            then liftIO (putStrLn ("skipped: needs chromium 143, found " <> unpack version))
            else do
              let
                json :: Text
                json = "{\"foo\":\"not bar\"}"
                simple = prefix <> "/simple.json"
                -- Each worker fetches the intercepted JSON and logs it.
                fetchWorker =
                  "new Worker(URL.createObjectURL(new Blob(['fetch(\"" <> simple <> "\").then(response => response.text()).then(t => console.log(t.trim()));'], { type: 'application/javascript' })))"
              route page (UrlMatchGlob "**/simple.json") $ \handled ->
                fulfillRoute
                  handled
                  defaultFulfillOptions
                    { fulfillStatus = Just (HttpStatus 200)
                    , fulfillBody = Just (FulfillText json)
                    , fulfillContentType = Just (ContentType "application/json")
                    }
              _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              messages <- newIORef []
              done <- newEmptyMVar
              onConsole page $ \message -> do
                modifyIORef' messages (consoleMessageText message :)
                count <- length <$> readIORef messages
                when (count == 2) (void (tryPutMVar done ()))
              _ <- evaluate page ("() => " <> fetchWorker)
              worker <- waitForWorker page
              _ <- evaluateOnWorker worker ("() => " <> fetchWorker)
              -- Bounded: the nested worker's log that never arrives must fail the test, not hang the run.
              outcome <- race (liftIO (threadDelay 30000000)) (takeMVar done)
              case outcome of
                Left () ->
                  assertFailure "nested worker messages never arrived"
                Right () -> do
                  received <- reverse <$> readIORef messages
                  assertEqual "messages" [json, json] received
    , testCase "should support extra http headers" $
        withAssets $ \prefix page -> do
          -- workerScriptLoaded is only reported from chromium 143.
          browser <- contextBrowser (pageContext page)
          version <- browserVersion browser
          let major = read (unpack (fst (breakOn "." version))) :: Int
          if major < 143
            then liftIO (putStrLn ("skipped: needs chromium 143, found " <> unpack version))
            else do
              setExtraHTTPHeaders page [HttpHeader (HeaderName "foo") (HeaderValue "bar")]
              let
                link = prefix <> "/one-style.css"
              -- Armed before navigation, and finished rather than started, since the header is only checked on a completed request.
              takeFinished <- armRequestFinished page
              withAsync (waitForWorker page) $ \handle -> do
                _ <- goto page (urlFromText (prefix <> "/worker/worker.html")) defaultGotoOptions
                worker <- wait handle
                takeStyle <- armRequest page (UrlMatchGlob link)
                _ <- evaluateOnWorker worker ("() => fetch('" <> link <> "')")
                scriptRequest <- awaitScriptFinished page takeFinished
                styleRequest <- takeStyle
                assertEqual "script header" (Just "bar") . headerNamed "foo" =<< requestAllHeaders scriptRequest
                assertEqual "fetch header" (Just "bar") . headerNamed "foo" =<< requestAllHeaders styleRequest
    , testCase "should support offline" $
        withAssets $ \prefix page -> do
          -- Chromium only: upstream marks webkit flaky and firefox does not plumb setOffline into WorkerNavigator.
          let
            ctx = pageContext page
          withAsync (waitForWorker page) $ \handle -> do
            _ <- goto page (urlFromText (prefix <> "/worker/worker.html")) defaultGotoOptions
            worker <- wait handle
            setOffline ctx True
            awaitWorkerText worker "() => String(navigator.onLine)" "false"
            fetched <-
              evaluateOnWorkerText
                worker
                ("() => fetch('" <> prefix <> "/one-style.css').then(() => 'ok', () => 'error')")
            assertEqual "fetch while offline" "error" fetched
            setOffline ctx False
            awaitWorkerText worker "() => String(navigator.onLine)" "true"
    , testCase "should resolve worker script allHeaders in main frame" $
        withAssets $ \prefix page -> do
          takeFinished <- armRequestFinished page
          _ <- goto page (urlFromText (prefix <> "/worker/worker.html")) defaultGotoOptions
          request <- awaitScriptFinished page takeFinished
          requestHeaders <- requestAllHeaders request
          assertBool "host header" (isJust (headerNamed "host" requestHeaders))
          response <- requestResponse request
          case response of
            Nothing ->
              assertFailure "worker.js request has no response"
            Just found -> do
              responseHeaders <- responseAllHeaders found
              assertBool "content-type header" (isJust (headerNamed "content-type" responseHeaders))
    , testCase "should resolve worker script allHeaders in iframe" $
        withAssets $ \prefix page -> do
          -- Needs Network.requestWillBeSentExtraInfo, which chromium only has from 151.
          browser <- contextBrowser (pageContext page)
          version <- browserVersion browser
          let major = read (unpack (fst (breakOn "." version))) :: Int
          if major < 151
            then liftIO (putStrLn ("skipped: needs chromium 151, found " <> unpack version))
            else do
              _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              takeFinished <- armRequestFinished page
              _ <- attachFrame page "frame1" (prefix <> "/worker/worker.html")
              request <- awaitScriptFinished page takeFinished
              requestHeaders <- requestAllHeaders request
              assertBool "host header" (isJust (headerNamed "host" requestHeaders))
              response <- requestResponse request
              case response of
                Nothing ->
                  assertFailure "worker.js request has no response"
                Just found -> do
                  responseHeaders <- responseAllHeaders found
                  assertBool "content-type header" (isJust (headerNamed "content-type" responseHeaders))
    , testCase "should resolve worker script allHeaders in nested worker inside iframe" $
        withAssets $ \prefix page -> do
          let
            scriptUrl = prefix <> "/worker/worker.js"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (waitForWorker page) $ \handle -> do
            takeFrameFinished <- armRequestFinished page
            _ <- attachFrame page "frame1" (prefix <> "/worker/worker.html")
            worker <- wait handle
            _ <- awaitScriptFinished page takeFrameFinished
            takeNestedFinished <- armRequestFinished page
            _ <- evaluateOnWorker worker ("() => { self.w = new Worker('" <> scriptUrl <> "'); }")
            request <- awaitScriptFinished page takeNestedFinished
            response <- requestResponse request
            case response of
              Nothing ->
                assertFailure "nested worker.js request has no response"
              Just found -> do
                headers <- responseAllHeaders found
                assertBool "content-type header" (isJust (headerNamed "content-type" headers))
    ]

-- | Waits for the finished request of the worker script. Another request can finish
-- first, such as the favicon, so those are skipped and the waiter is re-armed.
awaitScriptFinished :: Page -> Playwright Request -> Playwright Request
awaitScriptFinished page takeNext = do
  finished <- takeNext
  name <- urlToText <$> requestUrl finished
  if isInfixOf "worker.js" name
    then pure finished
    else do
      takeAgain <- armRequestFinished page
      awaitScriptFinished page takeAgain

-- | Finds a header by name, ignoring case, since the protocol may lowercase names.
headerNamed :: Text -> [HttpHeader] -> Maybe Text
headerNamed name headers =
  listToMaybe [value | HttpHeader (HeaderName key) (HeaderValue value) <- headers, toLower key == toLower name]

-- | Polls a worker expression until it returns the expected text, so a
-- network-state change that lags behind 'setOffline' does not fail the test.
awaitWorkerText :: Worker -> Text -> Text -> Playwright ()
awaitWorkerText worker expression expected = go (50 :: Int)
  where
    go :: Int -> Playwright ()
    go 0 = assertFailure ("worker never reported " <> unpack expected)
    go attempts = do
      actual <- evaluateOnWorkerText worker expression
      if actual == expected
        then pure ()
        else liftIO (threadDelay 100000) >> go (attempts - 1)

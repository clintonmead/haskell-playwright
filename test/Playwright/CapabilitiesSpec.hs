module Playwright.CapabilitiesSpec
  ( tests
  ) where

import Control.Concurrent
  ( Chan
  , newChan
  , readChan
  , writeChan
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text
  ( Text
  )
import Network.HTTP.Types
  ( HeaderName
  , RequestHeaders
  , hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , requestHeaders
  , responseLBS
  )
import Playwright
  ( ContentType (ContentType)
  , Exactness (Inexact)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , LoadState (LoadStateLoad)
  , UrlMatch (UrlMatchGlob)
  , WaitForState (WaitForVisible)
  , childLocator
  , click
  , defaultFulfillOptions
  , defaultGotoOptions
  , evaluate
  , evaluateBool
  , evaluateOnFrame
  , evaluateOnFrameText
  , evaluateText
  , evaluateTexts
  , fileChooserPage
  , frameLocator
  , frames
  , fulfillRoute
  , getByText
  , goto
  , innerText
  , isVisible
  , pageTextContent
  , pagesEqual
  , reload
  , route
  , selectorFromText
  , setContent
  , urlFromText
  , waitFor
  , waitForFileChooser
  , waitForFunction
  , waitForLoadState
  )
import Playwright.Harness
  ( locate
  , withAssetPage
  , withAssets
  , withChromiumPage
  , withLocalApp
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
import UnliftIO.Async
  ( wait
  , withAsync
  )

tests :: TestTree
tests =
  testGroup
    "capabilities"
    [ testCase "Web Assembly should work" $
        withAssetPage "wasm/table2.html" $ \page ->
          assertEqual "table" "42, 83" =<< evaluateText page "() => loadTable()"
    , testCase "should respect CSP" $
        withChromiumPage $ \page ->
          withLocalApp cspApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "status" "SUCCESS" =<< evaluateText page "() => window['testStatus']"
    , testCase "should support webgl" $
        withChromiumPage $ \page ->
          assertEqual "webgl" True
            =<< evaluateBool page "() => { const canvas = document.createElement('canvas'); return !!canvas.getContext('webgl'); }"
    , testCase "should support webgl 2" $
        withChromiumPage $ \page ->
          assertEqual "webgl2" True
            =<< evaluateBool page "() => { const canvas = document.createElement('canvas'); return !!canvas.getContext('webgl2'); }"
    , testCase "should not crash on storage.getDirectory()" $
        withAssetPage "empty.html" $ \page -> do
          _ <- evaluate page "async () => { const dir = await navigator.storage.getDirectory(); return dir.name; }"
          pure ()
    , testCase "navigator.clipboard should be present" $
        withAssetPage "empty.html" $ \page ->
          assertEqual "clipboard" True =<< evaluateBool page "() => !!navigator.clipboard"
    , testCase "serviceWorker should intercept document request" $
        withChromiumPage $ \page ->
          withLocalApp serviceWorkerApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <-
              evaluate
                page
                "async () => { await navigator.serviceWorker.register('/sw.js'); await new Promise(resolve => navigator.serviceWorker.oncontrollerchange = resolve); }"
            _ <- reload page
            assertEqual "body" (Just "intercepted") =<< pageTextContent page (selectorFromText "body")
    , testCase "make sure that XMLHttpRequest upload events are emitted correctly" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          events <-
            evaluateTexts
              page
              "async () => {\
              \  const events = [];\
              \  const xhr = new XMLHttpRequest();\
              \  xhr.upload.addEventListener('loadstart', () => events.push('loadstart'));\
              \  xhr.upload.addEventListener('progress', () => events.push('progress'));\
              \  xhr.upload.addEventListener('load', () => events.push('load'));\
              \  xhr.upload.addEventListener('loadend', () => events.push('loadend'));\
              \  xhr.open('POST', '/simple.json');\
              \  xhr.send('hello');\
              \  await new Promise(f => xhr.onload = f);\
              \  return events;\
              \}"
          assertEqual "events" ["loadstart", "progress", "load", "loadend"] events
    , testCase "loading in HTMLImageElement.prototype" $
        withAssetPage "empty.html" $ \page ->
          assertEqual "loading" True
            =<< evaluateBool page "() => 'loading' in HTMLImageElement.prototype"
    , testCase "window.GestureEvent in WebKit" $
        withAssetPage "empty.html" $ \page -> do
          assertEqual "defined" False =<< evaluateBool page "() => 'GestureEvent' in window"
          assertEqual "type" "undefined" =<< evaluateText page "() => typeof window.GestureEvent"
    , testCase "requestFullscreen" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => {\
              \  const result = new Promise(resolve => document.addEventListener('fullscreenchange', resolve));\
              \  void document.documentElement.requestFullscreen();\
              \  return result;\
              \}"
          assertEqual "fullscreen" True
            =<< evaluateBool page "() => document.fullscreenElement === document.documentElement"
          _ <-
            evaluate
              page
              "() => {\
              \  const result = new Promise(resolve => document.addEventListener('fullscreenchange', resolve));\
              \  void document.exitFullscreen();\
              \  return result;\
              \}"
          assertEqual "exited" False =<< evaluateBool page "() => !!document.fullscreenElement"
    , testCase "should send no Content-Length header for GET requests with a Content-Type" $
        withChromiumPage $ \page -> do
          captured <- liftIO newChan
          withLocalApp (contentLengthApp captured) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            withAsync (liftIO (readChan captured)) $ \pending -> do
              _ <-
                evaluate
                  page
                  "() => fetch('/empty.html', { headers: { 'Content-Type': 'application/json' }, method: 'GET' })"
              headers <- wait pending
              assertEqual "content-length" Nothing (lookupHeader "content-length" headers)
    , testCase "Intl.ListFormat should work" $
        withAssetPage "empty.html" $ \page ->
          assertEqual "list" "first, second, or third"
            =<< evaluateText
              page
              "() => {\
              \  const data = ['first', 'second', 'third'];\
              \  const listFormat = new Intl.ListFormat('en', { type: 'disjunction', style: 'short' });\
              \  return listFormat.format(data);\
              \}"
    , testCase "service worker should cover the iframe" serviceWorkerIframe
    , testCase "service worker should register in an iframe" serviceWorkerRegisterInIframe
    , testCase "should not crash when clicking a label with a <input type=\"file\"/>" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<form><label>A second file<input type=\"file\" /></label></form>"
          withAsync (waitForFileChooser page) $ \pending -> do
            click (getByText page "A second file" Inexact)
            chooser <- wait pending
            assertEqual "page" True (pagesEqual (fileChooserPage chooser) page)
    , testCase "should not crash when clicking a color input" $
        withChromiumPage $ \page -> do
          setContent page "<input type=\"color\">"
          let
            input = locate page "input"
          assertEqual "before" True =<< isVisible input
          click input
          assertEqual "after" True =<< isVisible input
    , testCase "should not auto play audio" $
        withChromiumPage $ \page -> do
          route page (UrlMatchGlob "**/*") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText autoPlayHtml)
                }
          _ <- goto page (urlFromText "http://127.0.0.1/audio.html") defaultGotoOptions
          _ <-
            waitForFunction
              page
              "() => { const log = document.getElementById('log'); return log && log.innerText === 'State: suspended'; }"
          pure ()
    , testCase "should not crash on feature detection for PublicKeyCredential" $
        withAssetPage "empty.html" $ \page -> do
          _ <-
            evaluate
              page
              "async () => {\
              \  await PublicKeyCredential.getClientCapabilities();\
              \  await PublicKeyCredential.isConditionalMediationAvailable();\
              \  await PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable();\
              \}"
          pure ()
    ]

serviceWorkerIframe :: IO ()
serviceWorkerIframe =
  withChromiumPage $ \page ->
    withLocalApp swIframeApp $ \prefix -> do
      _ <- goto page (urlFromText (prefix <> "/sw.html")) defaultGotoOptions
      _ <- evaluate page "() => window['activationPromise']"
      _ <-
        evaluate
          page
          "() => { const iframe = document.createElement('iframe'); iframe.src = '/iframe.html'; document.body.appendChild(iframe); }"
      let
        divLocator = childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "div")
      waitFor divLocator WaitForVisible
      assertEqual "text" "from the service worker" =<< innerText divLocator

serviceWorkerRegisterInIframe :: IO ()
serviceWorkerRegisterInIframe =
  withChromiumPage $ \page ->
    withLocalApp swRegisterApp $ \prefix -> do
      _ <- goto page (urlFromText (prefix <> "/main.html")) defaultGotoOptions
      waitForLoadState page LoadStateLoad
      allFrames <- frames page
      case allFrames of
        _ : iframe : _ -> do
          _ <- evaluateOnFrame iframe "() => window['activationPromise']"
          response <-
            evaluateOnFrameText
              iframe
              "async () => { const response = await fetch('foo.txt'); return response.text(); }"
          assertEqual "sw" "responseFromServiceWorker" response
        _ ->
          assertFailure ("expected iframe, got " <> show (length allFrames))

cspApp :: Application
cspApp _ respond =
  respond
    ( responseLBS
        status200
        [(hContentType, "text/html"), ("Content-Security-Policy", "script-src 'unsafe-inline';")]
        ( LBS8.pack
            "<script>window.testStatus = 'SUCCESS'; window.testStatus = eval(\"'FAILED'\");</script>"
        )
    )

serviceWorkerApp :: Application
serviceWorkerApp request respond =
  case pathInfo request of
    ["sw.js"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "application/javascript")]
            ( LBS8.pack
                "self.addEventListener('fetch', event => { event.respondWith(new Response('intercepted')); });\
                \self.addEventListener('activate', event => { event.waitUntil(clients.claim()); });"
            )
        )
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] (LBS8.pack "<html><body></body></html>"))

swIframeApp :: Application
swIframeApp request respond =
  case pathInfo request of
    ["sw.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            ( LBS8.pack
                "<script>window.registrationPromise = navigator.serviceWorker.register('sw.js');\
                \window.activationPromise = new Promise(resolve => navigator.serviceWorker.oncontrollerchange = resolve);</script>"
            )
        )
    ["iframe.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            (LBS8.pack "<div>from the server</div>")
        )
    ["sw.js"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "application/javascript")]
            ( LBS8.pack
                "const kIframeHtml = \"<div>from the service worker</div>\";\
                \self.addEventListener('fetch', event => {\
                \  if (event.request.url.endsWith('iframe.html')) {\
                \    const blob = new Blob([kIframeHtml], { type: 'text/html' });\
                \    event.respondWith(new Response(blob, { status: 200, statusText: 'OK' }));\
                \    return;\
                \  }\
                \  event.respondWith(fetch(event.request));\
                \});\
                \self.addEventListener('activate', event => { event.waitUntil(clients.claim()); });"
            )
        )
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)

swRegisterApp :: Application
swRegisterApp request respond =
  case pathInfo request of
    ["main.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            (LBS8.pack "<iframe src='/dir/iframe.html'></iframe>")
        )
    ["dir", "iframe.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            ( LBS8.pack
                "<script>window.registrationPromise = navigator.serviceWorker.register('sw.js');\
                \window.activationPromise = new Promise(resolve => navigator.serviceWorker.oncontrollerchange = resolve);</script>"
            )
        )
    ["dir", "sw.js"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "application/javascript")]
            ( LBS8.pack
                "self.addEventListener('fetch', event => {\
                \  if (event.request.url.endsWith('html')) {\
                \    event.respondWith(fetch(event.request));\
                \    return;\
                \  }\
                \  const blob = new Blob(['responseFromServiceWorker'], { type: 'text/plain' });\
                \  event.respondWith(new Response(blob, { status: 200, statusText: 'OK' }));\
                \});\
                \self.addEventListener('activate', event => { event.waitUntil(clients.claim()); });"
            )
        )
    _ ->
      respond (responseLBS status200 [(hContentType, "text/html")] LBS8.empty)

contentLengthApp :: Chan RequestHeaders -> Application
contentLengthApp captured request respond = do
  let
    jsonGet = lookup "content-type" (requestHeaders request) == Just "application/json"
  if jsonGet
    then writeChan captured (requestHeaders request)
    else pure ()
  respond (responseLBS status200 [(hContentType, "text/html")] (LBS8.pack "<html></html>"))

lookupHeader :: HeaderName -> RequestHeaders -> Maybe ByteString
lookupHeader =
  lookup

autoPlayHtml :: Text
autoPlayHtml =
  "<script>async function onLoad() {\
  \  const log = document.getElementById('log');\
  \  const audioContext = new AudioContext();\
  \  const gainNode = new GainNode(audioContext);\
  \  gainNode.connect(audioContext.destination);\
  \  gainNode.gain.value = 0.025;\
  \  const sineNode = new OscillatorNode(audioContext);\
  \  sineNode.connect(gainNode);\
  \  sineNode.start();\
  \  await new Promise((resolve) => setTimeout(resolve, 1000));\
  \  log.innerHTML = 'State: ' + audioContext.state;\
  \} </script><body onload=\"onLoad()\"><div id=\"log\"></div></body>"

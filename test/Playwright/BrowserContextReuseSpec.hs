module Playwright.BrowserContextReuseSpec
  ( tests
  ) where

-- Port of upstream library/browsercontext-reuse.spec.ts, launch scenario only.
--
-- Not ported:
--   * the connect scenario for every case (src has no launchServer or connect);
--   * "should reset tracing" (src has no tracing API);
--   * "should work with routeWebSocket" (src now has routeWebSocket, but this
--     case has not been ported yet; it is left for a follow-up port).

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( when
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( fromMaybe
  , isJust
  )
import Data.Text
  ( Text
  )
import Network.HTTP.Types
  ( Header
  , hCacheControl
  , hContentLength
  , hContentType
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , rawPathInfo
  , responseLBS
  )
import Playwright
  ( BindingName (BindingName)
  , BrowserContext
  , ClockTime (ClockTimeString)
  , ColorScheme (ColorSchemeDark, ColorSchemeLight)
  , NewContextOptions (newContextColorScheme, newContextViewport)
  , Page
  , Playwright
  , Point (Point)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , clockSetFixedTime
  , closePage
  , contextExposeFunction
  , contextPages
  , cssPixelFromInt
  , defaultGotoOptions
  , defaultNewContextOptions
  , disconnectFromReusedContext
  , evaluateBool
  , evaluateNumber
  , evaluateText
  , goto
  , mouseMove
  , newContextForReuseWith
  , newPage
  , numberArgument
  , reload
  , setContent
  , undefinedArgument
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , rafraf
  , withChromiumBrowser
  , withLocalApp
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber)
  , SerializedValue (SerializedNode)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( throwIO
  )
import UnliftIO.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )

tests :: TestTree
tests =
  testGroup
    "browsercontext-reuse"
    [ testGroup
        "reuse launch"
        [ testCase "should re-add binding after reset" $
            withReuse $ \reused -> do
              ctx1 <- reused defaultNewContextOptions
              _ <- contextExposeFunction ctx1 (BindingName "add") (calculate (-))
              page1 <- newPage ctx1
              assertEqual "subtract" (1 :: Double)
                =<< evaluateNumber page1 "async () => window.add(7, 6)"

              ctx2 <- reused defaultNewContextOptions
              _ <- contextExposeFunction ctx2 (BindingName "add") (calculate (+))
              page2 <- firstPage ctx2
              assertEqual "add" (11 :: Double)
                =<< evaluateNumber page2 "async () => window.add(5, 6)"
              _ <- reload page2
              assertEqual "add after reload" (11 :: Double)
                =<< evaluateNumber page2 "async () => window.add(5, 6)"
        , testCase "should reset serviceworker" $
            withReuse $ \reused ->
              withLocalApp (serviceWorkerApp False) $ \prefix -> do
                ctx1 <- reused defaultNewContextOptions
                page1 <- newPage ctx1
                _ <- goto page1 (urlFromText (prefix <> "/page.html")) defaultGotoOptions
                assertEqual "title" ("Page Title" :: Text) =<< evaluateText page1 "() => document.title"
                _ <- evaluateBool page1 "async () => { await window.activationPromise; return true; }"

                ctx2 <- reused defaultNewContextOptions
                page2 <- firstPage ctx2
                _ <- goto page2 (urlFromText (prefix <> "/page.html")) defaultGotoOptions
                assertEqual "title after reset" ("Page Title" :: Text)
                  =<< evaluateText page2 "() => document.title"
        , testCase "should reset serviceworker that hangs in importScripts" $
            withReuse $ \reused ->
              withLocalApp (serviceWorkerApp True) $ \prefix -> do
                ctx1 <- reused defaultNewContextOptions
                page1 <- newPage ctx1
                _ <- goto page1 (urlFromText (prefix <> "/page.html")) defaultGotoOptions
                assertEqual "title" ("Page Title" :: Text) =<< evaluateText page1 "() => document.title"

                ctx2 <- reused defaultNewContextOptions
                page2 <- firstPage ctx2
                _ <- goto page2 (urlFromText (prefix <> "/page.html")) defaultGotoOptions
                assertEqual "title after reset" ("Page Title" :: Text)
                  =<< evaluateText page2 "() => document.title"
        , testCase "should not cache resources" $ do
            counts <- newIORef ([] :: [(BS8.ByteString, Int)])
            withReuse $ \reused ->
              withLocalApp (cachingApp counts) $ \prefix -> do
                ctx1 <- reused defaultNewContextOptions
                page1 <- newPage ctx1
                _ <- goto page1 (urlFromText (prefix <> "/page.html")) defaultGotoOptions
                assertEqual "title" ("Count: 0" :: Text) =<< evaluateText page1 "() => document.title"
                assertCount counts "/page.html" 1
                assertCount counts "/style.css" 1
                assertCount counts "/simple.json" 1

                ctx2 <- reused defaultNewContextOptions
                page2 <- firstPage ctx2
                _ <- goto page2 (urlFromText (prefix <> "/page.html")) defaultGotoOptions
                assertEqual "title after reset" ("Count: 1" :: Text)
                  =<< evaluateText page2 "() => document.title"
                assertCount counts "/page.html" 2
                assertCount counts "/style.css" 2
                assertCount counts "/simple.json" 2
        , testCase "should ignore binding from beforeunload" $ do
            called <- newIORef False
            withReuse $ \reused -> do
              ctx1 <- reused defaultNewContextOptions
              _ <- contextExposeFunction ctx1 (BindingName "binding") $ \_ -> do
                liftIO (writeIORef called True)
                pure undefinedArgument
              page1 <- newPage ctx1
              _ <-
                evaluateBool
                  page1
                  "() => { window.addEventListener('beforeunload', () => window['binding']()); return true; }"

              ctx2 <- reused defaultNewContextOptions
              page2 <- firstPage ctx2
              setContent page2 "hello"
            wasCalled <- readIORef called
            assertBool "binding should not be called" (not wasCalled)
        , testCase "should reset mouse position" $
            withReuse $ \reused -> do
              ctx1 <- reused defaultNewContextOptions
              page1 <- newPage ctx1
              _ <- setContent page1 mousePageContent
              assertEqual "one blue" ("rgb(0, 0, 255)" :: Text) =<< backgroundOf page1 "one"
              assertEqual "two blue" ("rgb(0, 0, 255)" :: Text) =<< backgroundOf page1 "two"

              mouseMove page1 (Point (cssPixelFromInt 10) (cssPixelFromInt 75))
              rafraf page1 2
              assertEqual "one still blue" ("rgb(0, 0, 255)" :: Text) =<< backgroundOf page1 "one"
              assertEqual "two hovered red" ("rgb(255, 0, 0)" :: Text) =<< backgroundOf page1 "two"

              ctx2 <- reused defaultNewContextOptions
              page2 <- firstPage ctx2
              _ <- setContent page2 mousePageContent
              assertEqual "one blue after reset" ("rgb(0, 0, 255)" :: Text) =<< backgroundOf page2 "one"
              assertEqual "two blue after reset" ("rgb(0, 0, 255)" :: Text) =<< backgroundOf page2 "two"
        , testCase "should work with clock emulation" $
            withReuse $ \reused -> do
              ctx1 <- reused defaultNewContextOptions
              page1 <- newPage ctx1
              clockSetFixedTime page1 (ClockTimeString "2020-01-01T00:00:00.000Z")
              assertEqual "clock" ("2020-01-01T00:00:00.000Z" :: Text)
                =<< evaluateText page1 "() => new Date().toISOString()"

              ctx2 <- reused defaultNewContextOptions
              page2 <- firstPage ctx2
              clockSetFixedTime page2 (ClockTimeString "2020-01-01T00:00:00Z")
              assertEqual "clock after reset" ("2020-01-01T00:00:00.000Z" :: Text)
                =<< evaluateText page2 "() => new Date().toISOString()"
        , testCase "should continue issuing events after closing the reused page" $ do
            hits <- newIORef ([] :: [BS8.ByteString])
            withReuse $ \reused ->
              withLocalApp (loggingApp hits oneStyleRoutes) $ \prefix -> do
                ctx1 <- reused defaultNewContextOptions
                page1 <- newPage ctx1
                _ <- goto page1 (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
                assertCssRequests hits 1
                closePage page1

                ctx2 <- reused defaultNewContextOptions
                page2 <- firstPage ctx2
                _ <- goto page2 (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
                assertCssRequests hits 2
        , testCase "should update viewport and media" $
            withReuse $ \reused -> do
              ctx1 <-
                reused
                  defaultNewContextOptions
                    { newContextViewport = Just (ViewportSize (ViewportWidth 800) (ViewportHeight 600))
                    , newContextColorScheme = Just ColorSchemeDark
                    }
              page1 <- newPage ctx1
              assertBool "dark"
                =<< evaluateBool page1 "() => matchMedia('(prefers-color-scheme: dark)').matches"
              assertEqual "width" (800 :: Double) =<< evaluateNumber page1 "() => window.innerWidth"
              assertEqual "height" (600 :: Double) =<< evaluateNumber page1 "() => window.innerHeight"
              closePage page1

              ctx2 <-
                reused
                  defaultNewContextOptions
                    { newContextViewport = Just (ViewportSize (ViewportWidth 600) (ViewportHeight 800))
                    , newContextColorScheme = Just ColorSchemeLight
                    }
              page2 <- newPage ctx2
              assertBool "light"
                =<< evaluateBool page2 "() => matchMedia('(prefers-color-scheme: light)').matches"
              assertEqual "width after reset" (600 :: Double)
                =<< evaluateNumber page2 "() => window.innerWidth"
              assertEqual "height after reset" (800 :: Double)
                =<< evaluateNumber page2 "() => window.innerHeight"
        ]
    ]

-- | The launch scenario. Each call hands back a context kept for reuse; the
-- context returned by the previous call is disconnected first, as upstream's
-- launch @reusedContext@ does.
withReuse :: ((NewContextOptions -> Playwright BrowserContext) -> Playwright a) -> IO a
withReuse action =
  withChromiumBrowser $ \browser -> do
    current <- newIORef Nothing
    action $ \opts -> do
      previous <- readIORef current
      when (isJust previous) $
        disconnectFromReusedContext browser "reusedContext"
      ctx <- newContextForReuseWith browser opts
      writeIORef current (Just ctx)
      pure ctx

firstPage :: BrowserContext -> Playwright Page
firstPage ctx = do
  pages <- contextPages ctx
  case pages of
    page : _ -> pure page
    [] -> assertFailure "expected the reused context to keep a page"

-- | Two-number binding helper, as in the exposeFunction spec.
calculate :: (Double -> Double -> Double) -> [SerializedValue] -> Playwright SerializedArgument
calculate op args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (op left right))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

backgroundOf :: Page -> Text -> Playwright Text
backgroundOf page ident =
  evaluateText
    page
    ("() => getComputedStyle(document.getElementById('" <> ident <> "')).backgroundColor")

mousePageContent :: Text
mousePageContent =
  "<style>div { height: 30px; background: blue; } div:hover { background: red; } html, body { margin: 0; padding: 0; }</style>"
    <> "<div id=filler>one</div><div id=one>one</div><div id=two>two</div>"

-- | A served path with its headers and body.
type RouteEntry = (BS8.ByteString, ([Header], LBS8.ByteString))

-- | Serves the given routes by path, and answers 404 for anything else.
serveRoutes :: [RouteEntry] -> Application
serveRoutes routes request respond =
  case lookup (rawPathInfo request) routes of
    Just (headers, body) ->
      respond (responseLBS status200 headers body)
    Nothing ->
      respond (responseLBS status404 [] "")

serviceWorkerApp :: Bool -> Application
serviceWorkerApp hangingImport =
  serveRoutes
    [ ( "/page.html"
      , ( [(hContentType, "text/html")]
        , "<title>Page Title</title><script>navigator.serviceWorker.register('sw.js'); window.activationPromise = new Promise(resolve => navigator.serviceWorker.oncontrollerchange = resolve);</script>"
        )
      )
    , ( "/sw.js"
      , ( [(hContentType, "application/javascript")]
        , LBS8.pack
            ( (if hangingImport then "importScripts('helper.js');" else "")
                <> "self.addEventListener('fetch', event => {"
                <> " const blob = new Blob(['<title>Wrong Title</title>'], { type : 'text/html' });"
                <> " event.respondWith(new Response(blob, { status: 200, statusText: 'OK' }));"
                <> "});"
                <> "self.addEventListener('activate', event => { event.waitUntil(clients.claim()); });"
            )
        )
      )
    , ( "/helper.js"
      , ( [(hContentType, "application/javascript"), (hContentLength, "1000")]
        , "1"
        )
      )
    ]

-- | Counts hits per path, and serves a page whose title shows how many
-- times the page itself was requested.
cachingApp :: IORef [(BS8.ByteString, Int)] -> Application
cachingApp counts request respond = do
  seen <- readIORef counts
  let
    path = rawPathInfo request
    count = fromMaybe 0 (lookup path seen)
  modifyIORef' counts (\entries -> (path, count + 1) : filter ((/= path) . fst) entries)
  respond $
    case path of
      "/page.html" ->
        responseLBS
          status200
          [(hContentType, "text/html"), (hCacheControl, "max-age=3600")]
          (LBS8.pack (pageHtml count))
      "/style.css" ->
        responseLBS
          status200
          [(hContentType, "text/css"), (hCacheControl, "max-age=3600")]
          "body { background-color: red; }"
      "/simple.json" ->
        responseLBS
          status200
          [(hContentType, "application/json"), (hCacheControl, "max-age=3600")]
          "{ \"foo\": \"bar\" }"
      _ ->
        responseLBS status404 [] ""
  where
    pageHtml :: Int -> String
    pageHtml count =
      "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><title>Count: "
        <> show count
        <> "</title><link rel=\"stylesheet\" href=\"style.css\"><script>fetch('simple.json').then(() => {});</script></head></html>"

assertCount :: IORef [(BS8.ByteString, Int)] -> BS8.ByteString -> Int -> Playwright ()
assertCount counts path expected = do
  seen <- liftIO (readIORef counts)
  assertEqual (BS8.unpack path) expected (fromMaybe 0 (lookup path seen))

oneStyleRoutes :: [RouteEntry]
oneStyleRoutes =
  [ ( "/one-style.html"
    , ([(hContentType, "text/html")], "<link rel=\"stylesheet\" href=\"one-style.css\"><div>hello</div>")
    )
  , ( "/one-style.css"
    , ([(hContentType, "text/css")], "body { background-color: red; }")
    )
  ]

-- | Records every request path before serving the routes.
loggingApp :: IORef [BS8.ByteString] -> [RouteEntry] -> Application
loggingApp hits routes request respond = do
  modifyIORef' hits (rawPathInfo request :)
  serveRoutes routes request respond

-- | Waits (bounded) for the server to have seen the stylesheet request the
-- expected number of times, then asserts the count.
assertCssRequests :: IORef [BS8.ByteString] -> Int -> Playwright ()
assertCssRequests hits expected = do
  seen <- waitForCount (10 :: Int)
  assertEqual "css hits" expected seen
  where
    waitForCount :: Int -> Playwright Int
    waitForCount remaining = do
      current <- liftIO (length . filter (== "/one-style.css") <$> readIORef hits)
      if current >= expected || remaining <= 0
        then pure current
        else do
          liftIO (threadDelay 100000)
          waitForCount (remaining - 1)

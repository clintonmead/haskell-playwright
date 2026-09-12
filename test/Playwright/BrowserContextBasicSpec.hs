module Playwright.BrowserContextBasicSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( concurrently_
  , mapConcurrently
  , mapConcurrently_
  , wait
  , withAsync
  )
import Control.Concurrent.STM
  ( atomically
  , newTVarIO
  , readTVarIO
  , writeTVar
  )
import Control.Exception
  ( try
  )
import Control.Monad
  ( forM_
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , replace
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( AriaRole (AriaRole)
  , Browser
  , BrowserContext
  , ColorScheme (ColorSchemeDark, ColorSchemeLight)
  , GetByRoleOptions (getByRoleName)
  , HttpStatus (httpStatusToInt)
  , NewContextOptions (newContextColorScheme, newContextDeviceScaleFactor, newContextIsMobile, newContextJavaScriptEnabled, newContextNoDefaultViewport, newContextOffline, newContextViewport)
  , Page
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , browserContexts
  , browsersEqual
  , click
  , closeContext
  , closePage
  , contextBrowser
  , contextPages
  , contextsEqual
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , deviceScaleFactorFromInt
  , evaluateBool
  , evaluateNumber
  , evaluateOnFrameBool
  , evaluateOnLocator
  , evaluateText
  , evaluateTextWith
  , evaluateTexts
  , evaluateWith
  , frames
  , getByRole
  , goto
  , hover
  , isClosed
  , isVisible
  , mainFrame
  , newBrowserPage
  , newBrowserPageWith
  , newContext
  , newContextWith
  , newPage
  , onContextPage
  , pageClickSelector
  , pageContext
  , pagesEqual
  , responseStatus
  , selectorFromText
  , setContent
  , setOffline
  , textArgument
  , textContent
  , urlFromText
  , waitForContextClose
  , waitForContextPage
  , waitForFunction
  , waitForPopup
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetsPrefix
  , withChromiumBrowser
  , withChromiumContextWith
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
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
    "browsercontext-basic"
    [ testCase "should create new context" $
        withChromiumBrowser $ \browser -> do
          listed0 <- browserContexts browser
          assertEqual "empty" (0 :: Int) (length listed0)
          ctx <- newContext browser
          listed1 <- browserContexts browser
          assertEqual "one" True (listedContexts listed1 [ctx])
          owner <- contextBrowser ctx
          assertEqual "browser" True (browsersEqual owner browser)
          ctx2 <- newContext browser
          listed2 <- browserContexts browser
          assertEqual "two" True (listedContexts listed2 [ctx, ctx2])
          closeContext ctx
          listed3 <- browserContexts browser
          assertEqual "after close" True (listedContexts listed3 [ctx2])
          ownerAfter <- contextBrowser ctx
          assertEqual "browser after close" True (browsersEqual ownerAfter browser)
          closeContext ctx2
    , testCase "should be able to click across browser contexts" $
        withChromiumBrowser $ \browser -> do
          listed0 <- browserContexts browser
          assertEqual "empty" (0 :: Int) (length listed0)
          page1 <- createClickPage browser
          page2 <- createClickPage browser
          let
            clickCount :: Int
            clickCount = 20
          concurrently_
            (clickInPage page1 clickCount)
            (clickInPage page2 clickCount)
          assertEqual "page1" (fromIntegral clickCount :: Double)
            =<< evaluateNumber page1 "() => window.clicks"
          assertEqual "page2" (fromIntegral clickCount :: Double)
            =<< evaluateNumber page2 "() => window.clicks"
          closePage page1
          closePage page2
    , localOption (mkTimeout 30000000) $
        testCase "should be able to hover across browser contexts in parallel" $
          withChromiumBrowser $ \browser -> do
            pages <-
              mapConcurrently
                ( \_ -> do
                    ctx <- newContext browser
                    page <- newPage ctx
                    setContent page hoverHtml
                    pure page
                )
                ([1, 2, 3, 4, 5] :: [Int])
            mapConcurrently_ hoverAndAssert pages
            mapConcurrently_ (closeContext . pageContext) pages
    , testCase "window.open should use parent tab context" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            ctx <- newContext browser
            page <- newPage ctx
            let
              empty = prefix <> "/empty.html"
            _ <- goto page (urlFromText empty) defaultGotoOptions
            withAsync (waitForPopup page) $ \handle -> do
              _ <- evaluateWith page "url => window.open(url)" (textArgument empty)
              popup <- wait handle
              assertEqual "popup context" True (contextsEqual (pageContext popup) ctx)
            closeContext ctx
    , testCase "should isolate localStorage and cookies" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            ctx1 <- newContext browser
            ctx2 <- newContext browser
            assertEqual "ctx1 empty" (0 :: Int) . length =<< contextPages ctx1
            assertEqual "ctx2 empty" (0 :: Int) . length =<< contextPages ctx2
            page1 <- newPage ctx1
            _ <- goto page1 (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <- evaluateText page1 "() => { localStorage.setItem('name', 'page1'); document.cookie = 'name=page1'; return 'ok'; }"
            assertEqual "ctx1 one" (1 :: Int) . length =<< contextPages ctx1
            assertEqual "ctx2 still empty" (0 :: Int) . length =<< contextPages ctx2
            page2 <- newPage ctx2
            _ <- goto page2 (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            _ <- evaluateText page2 "() => { localStorage.setItem('name', 'page2'); document.cookie = 'name=page2'; return 'ok'; }"
            pages1 <- contextPages ctx1
            pages2 <- contextPages ctx2
            assertEqual "ctx1 count" (1 :: Int) (length pages1)
            assertEqual "ctx2 count" (1 :: Int) (length pages2)
            assertEqual "page1 listed" True (any (pagesEqual page1) pages1)
            assertEqual "page2 listed" True (any (pagesEqual page2) pages2)
            assertEqual "ls1" "page1" =<< evaluateText page1 "() => localStorage.getItem('name')"
            assertEqual "cookie1" "name=page1" =<< evaluateText page1 "() => document.cookie"
            assertEqual "ls2" "page2" =<< evaluateText page2 "() => localStorage.getItem('name')"
            assertEqual "cookie2" "name=page2" =<< evaluateText page2 "() => document.cookie"
            concurrently_ (closeContext ctx1) (closeContext ctx2)
            listed <- browserContexts browser
            assertEqual "closed" (0 :: Int) (length listed)
    , testCase "should propagate default viewport to the page" $
        withChromiumContextWith
          defaultNewContextOptions
            { newContextViewport =
                Just (ViewportSize (ViewportWidth 456) (ViewportHeight 789))
            }
          $ \_ page -> do
            assertEqual "width" (456 :: Double)
              =<< evaluateNumber page "() => window.innerWidth"
            assertEqual "height" (789 :: Double)
              =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should make a copy of default viewport" $
        withChromiumContextWith
          defaultNewContextOptions
            { newContextViewport =
                Just (ViewportSize (ViewportWidth 456) (ViewportHeight 789))
            }
          $ \_ page -> do
            assertEqual "width" (456 :: Double)
              =<< evaluateNumber page "() => window.innerWidth"
            assertEqual "height" (789 :: Double)
              =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should respect deviceScaleFactor" $
        withChromiumContextWith
          defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 3)}
          $ \_ page ->
            assertEqual "ratio" (3 :: Double)
              =<< evaluateNumber page "() => window.devicePixelRatio"
    , testCase "should not allow deviceScaleFactor with null viewport" $
        withChromiumBrowser $ \browser -> do
          result <-
            try
              ( newContextWith
                  browser
                  defaultNewContextOptions
                    { newContextNoDefaultViewport = Just True
                    , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 1)
                    }
              )
          case result of
            Left (DecodeFailure message) ->
              assertBool
                "message"
                ("\"deviceScaleFactor\" option is not supported with null \"viewport\"" `isInfixOf` message)
            Left err ->
              assertFailure ("expected DecodeFailure, got: " <> show err)
            Right _ ->
              assertFailure "expected DecodeFailure"
    , testCase "should not allow isMobile with null viewport" $
        withChromiumBrowser $ \browser -> do
          result <-
            try
              ( newContextWith
                  browser
                  defaultNewContextOptions
                    { newContextNoDefaultViewport = Just True
                    , newContextIsMobile = Just True
                    }
              )
          case result of
            Left (DecodeFailure message) ->
              assertBool
                "message"
                ("\"isMobile\" option is not supported with null \"viewport\"" `isInfixOf` message)
            Left err ->
              assertFailure ("expected DecodeFailure, got: " <> show err)
            Right _ ->
              assertFailure "expected DecodeFailure"
    , testCase "close() should work for empty context" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          closeContext ctx
    , testCase "close() should abort waitForEvent" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          assertProtocolContains
            "Target page, context or browser has been closed"
            ( withAsync (waitForContextPage ctx) $ \handle -> do
                closeContext ctx
                wait handle
            )
    , testCase "close() should be callable twice" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          closeContext ctx
          closeContext ctx
    , testCase "should pass self to close event" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          withAsync (waitForContextClose ctx) $ \handle -> do
            closeContext ctx
            closed <- wait handle
            assertEqual "self" True (contextsEqual closed ctx)
    , testCase "should not report frameless pages on error" $
        withApplication (pure popupLinkApp) $ \port ->
          withChromiumBrowser $ \browser -> do
            let
              prefix = "http://127.0.0.1:" <> pack (show port)
              empty = prefix <> "/empty.html"
            ctx <- newContext browser
            page <- newPage ctx
            popupVar <- newTVarIO Nothing
            onContextPage ctx $ \opened ->
              atomically (writeTVar popupVar (Just opened))
            _ <- goto page (urlFromText empty) defaultGotoOptions
            pageClickSelector page (selectorFromText "\"Click me\"")
            closeContext ctx
            popup <- readTVarIO popupVar
            case popup of
              Nothing ->
                pure ()
              Just opened -> do
                assertEqual "closed" True =<< isClosed opened
                case mainFrame opened of
                  _ ->
                    pure ()
    , testCase "should return all of the pages" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          page <- newPage ctx
          second <- newPage ctx
          allPages <- contextPages ctx
          assertEqual "count" (2 :: Int) (length allPages)
          assertEqual "page" True (any (pagesEqual page) allPages)
          assertEqual "second" True (any (pagesEqual second) allPages)
          closeContext ctx
    , testCase "should close all belonging pages once closing context" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          _ <- newPage ctx
          listed <- contextPages ctx
          assertEqual "one" (1 :: Int) (length listed)
          closeContext ctx
          after <- contextPages ctx
          assertEqual "none" (0 :: Int) (length after)
    , testCase "should disable javascript" $
        withChromiumContextWith
          defaultNewContextOptions {newContextJavaScriptEnabled = Just False}
          $ \_ page -> do
            _ <-
              goto
                page
                (urlFromText "data:text/html, <script>var something = \"forbidden\"</script>")
                defaultGotoOptions
            assertProtocolContains
              "something is not defined"
              (evaluateText page "() => something")
    , testCase "should run scripts when javascript is enabled" $
        withChromiumContextWith defaultNewContextOptions $ \_ page -> do
          _ <-
            goto
              page
              (urlFromText "data:text/html, <script>var something = \"forbidden\"</script>")
              defaultGotoOptions
          assertEqual "value" "forbidden" =<< evaluateText page "() => something"
    , testCase "should be able to navigate after disabling javascript" $
        withAssetsPrefix $ \prefix ->
          withChromiumContextWith
            defaultNewContextOptions {newContextJavaScriptEnabled = Just False}
            $ \_ page -> do
              _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              pure ()
    , testCase "should not hang on promises after disabling javascript" $
        withChromiumContextWith
          defaultNewContextOptions {newContextJavaScriptEnabled = Just False}
          $ \_ page -> do
            assertEqual "sync" (1 :: Double) =<< evaluateNumber page "() => 1"
            assertEqual "async" (2 :: Double) =<< evaluateNumber page "async () => 2"
    , testCase "setContent should work after disabling javascript" $
        withChromiumContextWith
          defaultNewContextOptions {newContextJavaScriptEnabled = Just False}
          $ \_ page -> do
            setContent page "<h1>Hello</h1>"
            assertEqual "text" (Just "Hello") =<< textContent (locate page "h1")
    , testCase "should work with offline option" $
        withAssetsPrefix $ \prefix ->
          withChromiumContextWith defaultNewContextOptions {newContextOffline = Just True} $
            \ctx page -> do
              assertProtocolContains
                "ERR_INTERNET_DISCONNECTED"
                (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)
              setOffline ctx False
              response <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              case response of
                Nothing ->
                  assertFailure "missing response"
                Just value ->
                  assertEqual "status" 200 . httpStatusToInt =<< responseStatus value
    , testCase "fetch with keepalive should throw when offline" $
        withApplication (pure fetchKeepaliveApp) $ \port ->
          withChromiumBrowser $ \browser -> do
            let
              prefix = "http://127.0.0.1:" <> pack (show port)
              fetchUrl = prefix <> "/fetch"
            ctx <- newContext browser
            page <- newPage ctx
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            assertEqual "ok" "hello"
              =<< evaluateTextWith
                page
                "url => fetch(url, { cache: 'no-store', keepalive: true }).then(response => response.json())"
                (textArgument fetchUrl)
            setOffline ctx True
            assertEqual "offline" "error"
              =<< evaluateTextWith
                page
                "async url => { try { const response = await fetch(url, { cache: 'no-store', keepalive: true }); return await response.json(); } catch { return 'error'; } }"
                (textArgument fetchUrl)
            closeContext ctx
    , testCase "should emulate navigator.onLine" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          page <- newPage ctx
          assertEqual "online" True =<< evaluateBool page "() => window.navigator.onLine"
          setOffline ctx True
          assertEqual "offline" False =<< evaluateBool page "() => window.navigator.onLine"
          setOffline ctx False
          assertEqual "online again" True =<< evaluateBool page "() => window.navigator.onLine"
          closeContext ctx
    , testCase "should emulate offline event" $
        withChromiumBrowser $ \browser -> do
          ctx <- newContext browser
          page <- newPage ctx
          _ <-
            evaluateText
              page
              "() => { window.__events = []; window.addEventListener('offline', () => window.__events.push('offline')); window.addEventListener('online', () => window.__events.push('online')); return 'ok'; }"
          setOffline ctx True
          _ <- waitForFunction page "() => window.__events.length === 1"
          assertEqual "offline" ["offline" :: Text]
            =<< evaluateTexts page "() => window.__events"
          setOffline ctx False
          _ <- waitForFunction page "() => window.__events.length === 2"
          assertEqual "online" ["offline" :: Text, "online"]
            =<< evaluateTexts page "() => window.__events"
          closeContext ctx
    , testCase "should emulate media in popup" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            let
              empty = prefix <> "/empty.html"
            ctx <-
              newContextWith
                browser
                defaultNewContextOptions {newContextColorScheme = Just ColorSchemeDark}
            page <- newPage ctx
            _ <- goto page (urlFromText empty) defaultGotoOptions
            withAsync (waitForPopup page) $ \handle -> do
              _ <- evaluateWith page "url => { window.open(url); }" (textArgument empty)
              popup <- wait handle
              assertEqual "not light" False
                =<< evaluateBool popup "() => matchMedia('(prefers-color-scheme: light)').matches"
              assertEqual "dark" True
                =<< evaluateBool popup "() => matchMedia('(prefers-color-scheme: dark)').matches"
            closeContext ctx
            lightPage <-
              newBrowserPageWith
                browser
                defaultNewContextOptions {newContextColorScheme = Just ColorSchemeLight}
            _ <- goto lightPage (urlFromText empty) defaultGotoOptions
            withAsync (waitForPopup lightPage) $ \handle -> do
              _ <- evaluateWith lightPage "url => { window.open(url); }" (textArgument empty)
              popup <- wait handle
              assertEqual "light" True
                =<< evaluateBool popup "() => matchMedia('(prefers-color-scheme: light)').matches"
              assertEqual "not dark" False
                =<< evaluateBool popup "() => matchMedia('(prefers-color-scheme: dark)').matches"
            closePage lightPage
    , testCase "should emulate media in cross-process iframe" $
        withAssetsPrefix $ \prefix ->
          withChromiumBrowser $ \browser -> do
            page <-
              newBrowserPageWith
                browser
                defaultNewContextOptions {newContextColorScheme = Just ColorSchemeDark}
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            let
              cross = replace "127.0.0.1" "localhost" prefix <> "/empty.html"
            _ <-
              evaluateWith
                page
                "url => { const frame = document.createElement('iframe'); frame.src = url; frame.id = 'frame1'; document.body.appendChild(frame); return new Promise(resolve => { frame.onload = resolve; }); }"
                (textArgument cross)
            allFrames <- frames page
            case allFrames of
              [_, child] ->
                assertEqual "dark" True
                  =<< evaluateOnFrameBool child "() => matchMedia('(prefers-color-scheme: dark)').matches"
              _ ->
                assertFailure ("expected iframe, got " <> show (length allFrames) <> " frames")
            closePage page
    , testCase "should create two pages in parallel in various contexts" $
        withChromiumBrowser $ \browser -> do
          ctx1 <- newContext browser
          ctx2 <- newContext browser
          _ <- mapConcurrently newPage [ctx1, ctx1, ctx2, ctx2]
          closeContext ctx1
          closeContext ctx2
          ctx3 <- newContext browser
          _ <- mapConcurrently newPage [ctx3, ctx3]
          closeContext ctx3
    ]

listedContexts :: [BrowserContext] -> [BrowserContext] -> Bool
listedContexts actual expected =
  length actual == length expected
    && and (zipWith contextsEqual actual expected)

createClickPage :: Browser -> IO Page
createClickPage browser = do
  page <- newBrowserPage browser
  setContent page "<button>Click me</button>"
  _ <-
    evaluateOnLocator
      (locate page "button")
      "button => { window.clicks = 0; button.addEventListener('click', () => ++window.clicks, false); }"
  pure page

clickInPage :: Page -> Int -> IO ()
clickInPage page count =
  forM_ [1 .. count] $ \_ ->
    click (locate page "button")

hoverHtml :: Text
hoverHtml =
  "<style>[role=\"tooltip\"] { display: none; } [role=\"tooltip\"].visible { display: block; }</style><button>Hover me</button><div role=\"tooltip\">Tooltip content</div><script>const button = document.querySelector('button'); const tooltip = document.querySelector('[role=\"tooltip\"]'); button.addEventListener('pointerenter', () => tooltip.classList.add('visible')); button.addEventListener('pointerleave', () => tooltip.classList.remove('visible'));</script>"

hoverAndAssert :: Page -> IO ()
hoverAndAssert page = do
  hover
    ( getByRole
        page
        (AriaRole "button")
        defaultGetByRoleOptions {getByRoleName = Just "Hover me"}
    )
  assertEqual "tooltip" True
    =<< isVisible (getByRole page (AriaRole "tooltip") defaultGetByRoleOptions)

popupLinkApp :: Application
popupLinkApp _req respond =
  respond
    ( responseLBS
        status200
        [(hContentType, "text/html")]
        "<a href=\"/empty.html\" target=\"_blank\">Click me</a>"
    )

fetchKeepaliveApp :: Application
fetchKeepaliveApp req respond =
  case pathInfo req of
    ["fetch"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "application/json")]
            "\"hello\""
        )
    _ ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<html></html>"
        )

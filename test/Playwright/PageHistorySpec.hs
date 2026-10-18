module Playwright.PageHistorySpec
  ( tests
  ) where

import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.Exception
  ( catch
  )
import UnliftIO.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )
import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forever
  , void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , replace
  , toLower
  )
import Data.ByteString.Lazy
  ( fromStrict
  )
import Data.Text.Encoding
  ( encodeUtf8
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , status200
  , status302
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Playwright
  ( ConsoleMessage (consoleMessageText)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody)
  , LoadState (LoadStateLoad)
  , Page
  , Playwright
  , SpecialJsValue (SpecialUndefined)
  , UrlGlob (UrlGlob)
  , WaitForState (WaitForVisible)
  , click
  , content
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultScreenshotOptions
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateSpecial
  , fulfillRoute
  , goBack
  , goForward
  , goto
  , innerText
  , reload
  , responseOk
  , responseText
  , responseUrl
  , route
  , screenshot
  , selectorFromText
  , setContent
  , url
  , urlFromText
  , urlToText
  , waitForConsoleWhere
  , waitForLoadState
  , waitForPopup
  , waitForRequest
  , waitForSelector
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( locate
  , testAssetsDir
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

tests :: TestTree
tests =
  testGroup
    "page-history"
    [ testCase "page.goBack should work" $
        withAssets $ \prefix page -> do
          none <- goBack page
          case none of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no history"
          let
            empty :: Text
            empty = prefix <> "/empty.html"
            grid :: Text
            grid = prefix <> "/grid.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          _ <- goto page (urlFromText grid) defaultGotoOptions
          back <- goBack page
          case back of
            Nothing ->
              assertFailure "expected goBack response"
            Just response -> do
              assertEqual "ok" True =<< responseOk response
              responseLocation <- urlToText <$> responseUrl response
              assertEqual "empty url" True (empty `isInfixOf` responseLocation)
          forward <- goForward page
          case forward of
            Nothing ->
              assertFailure "expected goForward response"
            Just response -> do
              assertEqual "ok" True =<< responseOk response
              responseLocation <- urlToText <$> responseUrl response
              assertEqual "grid url" True ("/grid.html" `isInfixOf` responseLocation)
          noneAgain <- goForward page
          case noneAgain of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected end of history"
    , testCase "page.goBack should work with HistoryAPI" $
        withAssets $ \prefix page -> do
          let
            empty :: Text
            empty = prefix <> "/empty.html"
          _ <- goto page (urlFromText empty) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { history.pushState({}, '', '/first.html'); history.pushState({}, '', '/second.html'); }"
          assertEqual "second" (prefix <> "/second.html") =<< (urlToText <$> url page)
          _ <- goBack page
          assertEqual "first" (prefix <> "/first.html") =<< (urlToText <$> url page)
          _ <- goBack page
          assertEqual "empty" empty =<< (urlToText <$> url page)
          _ <- goForward page
          assertEqual "first again" (prefix <> "/first.html") =<< (urlToText <$> url page)
    , testCase "page.goBack should work for file urls" $
        withAssets $ \prefix page -> do
          url1 <- assetFileUrl "consolelog.html"
          let
            url2 = prefix <> "/consolelog.html"
          _ <- withAsync (waitForConsoleWhere page (\message -> consoleMessageText message == "here:" <> url1)) $ \pending -> do
            _ <- goto page (urlFromText url1) defaultGotoOptions
            wait pending
          setContent page ("<a href='" <> url2 <> "'>url2</a>")
          assertEqual "file url" (toLower url1) . toLower . urlToText =<< url page
          _ <- withAsync (waitForConsoleWhere page (\message -> consoleMessageText message == "here:" <> url2)) $ \pending -> do
            click (locate page "a")
            wait pending
          assertEqual "server url" url2 . urlToText =<< url page
          _ <- withAsync (waitForConsoleWhere page (\message -> consoleMessageText message == "here:" <> url1)) $ \pending -> do
            _ <- goBack page
            wait pending
          assertEqual "back file" (toLower url1) . toLower . urlToText =<< url page
          assertEqual "scrollX" (0 :: Double) =<< evaluateNumber page "() => window.scrollX"
          _ <- screenshot page defaultScreenshotOptions
          _ <- withAsync (waitForConsoleWhere page (\message -> consoleMessageText message == "here:" <> url2)) $ \pending -> do
            _ <- goForward page
            wait pending
          assertEqual "forward server" url2 . urlToText =<< url page
          assertEqual "scrollX forward" (0 :: Double) =<< evaluateNumber page "() => window.scrollX"
          _ <- screenshot page defaultScreenshotOptions
          pure ()
    , testCase "goBack/goForward should work with bfcache-able pages" $
        withAssets $ \prefix page -> do
          let
            cached = prefix <> "/cached/bfcached.html"
            cachedFoo = cached <> "?foo"
          _ <- goto page (urlFromText cached) defaultGotoOptions
          setContent page ("<a href=" <> pack (show cachedFoo) <> ">click me</a>")
          click (locate page "a")
          back <- goBack page
          case back of
            Nothing ->
              assertFailure "expected goBack response"
            Just response ->
              assertEqual "back url" cached . urlToText =<< responseUrl response
          assertEqual "not persisted" True
            =<< evaluateBool page "async () => (await window.didShow).persisted === false"
          forward <- goForward page
          case forward of
            Nothing ->
              assertFailure "expected goForward response"
            Just response ->
              assertEqual "forward url" cachedFoo . urlToText =<< responseUrl response
    , testCase "page.reload should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "() => { window._foo = 10; }"
          _ <- reload page
          special <- evaluateSpecial page "() => window._foo"
          assertEqual "cleared" SpecialUndefined special
    , testCase "page.reload should work with data url" $
        withChromiumPage $ \page -> do
          _ <- goto page (urlFromText "data:text/html,hello") defaultGotoOptions
          assertEqual "hello" True . ("hello" `isInfixOf`) =<< content page
          response <- reload page
          case response of
            Nothing ->
              pure ()
            Just _ ->
              assertFailure "expected no response"
          assertEqual "hello after" True . ("hello" `isInfixOf`) =<< content page
    , testCase "page.reload during renderer-initiated navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          runRendererNavigation page (void (reload page))
    , testCase "page.reload should not resolve with same-document navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "() => 1"
          arrived <- newEmptyMVar
          release <- newEmptyMVar
          route page (UrlGlob (prefix <> "/empty.html")) $ \handled -> do
            putMVar arrived ()
            takeMVar release
            fulfillRoute handled defaultFulfillOptions {fulfillBody = Just (FulfillText "hello")}
          swallow (evaluate page "() => { window.history.pushState({}, ''); }")
          withAsync (reload page) $ \pending -> do
            swallow (evaluate page "() => { window.history.pushState({}, ''); }")
            -- Bounded: if the reload never reaches the route, fail instead of hanging the run.
            arrivedInTime <- race (liftIO (threadDelay 30000000)) (takeMVar arrived)
            case arrivedInTime of
              Left () ->
                assertFailure "reload never reached the route"
              Right () ->
                pure ()
            putMVar release ()
            response <- wait pending
            case response of
              Nothing ->
                assertFailure "expected reload response"
              Just got ->
                assertEqual "body" "hello" =<< responseText got
    , testCase "page.reload should work with same origin redirect" $
        reloadRedirect False
    , testCase "page.reload should work with cross-origin redirect" $
        reloadRedirect True
    , testCase "page.reload should work on a page with a hash" $
        withAssets $ \prefix page -> do
          let hashed = prefix <> "/empty.html#hash"
          _ <- goto page (urlFromText hashed) defaultGotoOptions
          _ <- reload page
          assertEqual "hash" hashed . urlToText =<< url page
    , testCase "page.reload should work on a page with a hash at the end" $
        withAssets $ \prefix page -> do
          let hashed = prefix <> "/empty.html#"
          _ <- goto page (urlFromText hashed) defaultGotoOptions
          _ <- reload page
          assertEqual "hash" hashed . urlToText =<< url page
    , testCase "page.goBack during renderer-initiated navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          runRendererNavigation page (void (goBack page))
    , testCase "page.goForward during renderer-initiated navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          _ <- goBack page
          runRendererNavigation page (void (goForward page))
    , testCase "regression test for issue 20791" $
        withChromiumPage $ \page ->
          withLocalApp issue20791App $ \prefix -> do
            _ <- withAsync (waitForConsoleWhere page (\message -> consoleMessageText message == "foo")) $ \firstFoo -> do
              _ <- goto page (urlFromText (prefix <> "/main.html")) defaultGotoOptions
              wait firstFoo
            withAsync (waitForConsoleWhere page (\message -> consoleMessageText message == "foo")) $ \secondFoo -> do
              _ <- reload page
              _ <- wait secondFoo
              pure ()
    , testCase "should reload proper page" $
        withChromiumPage $ \page -> do
          mainRequest <- newIORef (0 :: Int)
          popupRequest <- newIORef (0 :: Int)
          withLocalApp (reloadProperApp mainRequest popupRequest) $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/main.html")) defaultGotoOptions
            popup <-
              withAsync (waitForPopup page) $ \pending -> do
                _ <- evaluate page "() => window.open('/popup.html')"
                wait pending
            waitForLoadState popup LoadStateLoad
            assertEqual "main 1" "main: 1" =<< innerText (locate page "h1")
            assertEqual "popup 1" "popup: 1" =<< innerText (locate popup "h1")
            _ <- reload page
            assertEqual "main 2" "main: 2" =<< innerText (locate page "h1")
            assertEqual "popup still 1" "popup: 1" =<< innerText (locate popup "h1")
            _ <- reload popup
            assertEqual "main still 2" "main: 2" =<< innerText (locate page "h1")
            assertEqual "popup 2" "popup: 2" =<< innerText (locate popup "h1")
    ]

reloadRedirect :: Bool -> IO ()
reloadRedirect cross =
  withChromiumPage $ \page -> do
    served <- newIORef False
    destination <- newIORef ("" :: Text)
    withLocalApp (reloadRedirectApp served destination) $ \prefix -> do
      let
        expected =
          ( if cross
              then replace "127.0.0.1" "localhost" prefix
              else prefix
          )
            <> "/title.html"
      writeIORef destination expected
      _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
      _ <- reload page
      assertEqual "url" expected . urlToText =<< url page

reloadRedirectApp :: IORef Bool -> IORef Text -> Application
reloadRedirectApp served destination request respond =
  case pathInfo request of
    ["empty.html"] -> do
      already <- readIORef served
      if already
        then do
          location <- readIORef destination
          respond (responseLBS status302 [(hLocation, encodeUtf8 location)] "")
        else do
          writeIORef served True
          respond (responseLBS status200 [(hContentType, "text/html")] "")
    ["title.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<title>t</title>")
    _ ->
      respond (responseLBS status200 [] "")

assetFileUrl :: FilePath -> Playwright Text
assetFileUrl relative = do
  dir <- testAssetsDir
  pure ("file://" <> pack dir <> "/" <> pack relative)

swallow :: Playwright a -> Playwright ()
swallow action =
  catch (void action) ignorePlaywrightError

ignorePlaywrightError :: PlaywrightError -> Playwright ()
ignorePlaywrightError _ =
  pure ()

runRendererNavigation :: Page -> Playwright () -> Playwright ()
runRendererNavigation page navigate = do
  route page (UrlGlob "**/post") $ \_ ->
    liftIO (forever (threadDelay 1000000))
  setContent page "<form method='POST' action='/post'>Form is here<input type='submit'></form>"
  withAsync
    ( do
        _ <- waitForRequest page (UrlGlob "**/post")
        swallow navigate
    )
    $ \pending -> do
      swallow (click (locate page "input[type=submit]"))
      wait pending
  waitForSelector page (selectorFromText "text=hello") WaitForVisible

issue20791App :: Application
issue20791App request respond =
  case pathInfo request of
    ["iframe.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            "<!doctype html><script type=\"text/javascript\">console.log(window.parent.foo);</script>"
        )
    ["main.html"] ->
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            "<!doctype html><iframe id=myframe src=\"about:blank\"></iframe><script type=\"text/javascript\">setTimeout(() => window.foo = 'foo', 0); setTimeout(() => myframe.contentDocument.location.href = location.origin + '/iframe.html', 0);</script>"
        )
    _ ->
      respond (responseLBS status200 [] "")

reloadProperApp :: IORef Int -> IORef Int -> Application
reloadProperApp mainRequest popupRequest request respond =
  case pathInfo request of
    ["main.html"] -> do
      n <- increment mainRequest
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (fromStrict (encodeUtf8 ("<!doctype html><h1>main: " <> pack (show n) <> "</h1>")))
        )
    ["popup.html"] -> do
      n <- increment popupRequest
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html; charset=utf-8")]
            (fromStrict (encodeUtf8 ("<!doctype html><h1>popup: " <> pack (show n) <> "</h1>")))
        )
    _ ->
      respond (responseLBS status200 [] "")

increment :: IORef Int -> IO Int
increment ref = do
  n <- readIORef ref
  let next = n + 1
  writeIORef ref next
  pure next

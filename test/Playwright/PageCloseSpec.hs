module Playwright.PageCloseSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( forever
  , void
  )
import Data.ByteString.Builder
  ( byteString
  )
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( isInfixOf
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  , responseStream
  )
import Playwright
  ( BindingName (BindingName)
  , GotoOptions (gotoOptionsWaitUntil)
  , LoadState (LoadStateDomContentLoaded)
  , Page
  , Playwright
  , Point (Point)
  , Response
  , UrlGlob (UrlGlob)
  , bringToFront
  , click
  , closePage
  , continueRoute
  , cssPixelFromInt
  , defaultGotoOptions
  , evaluate
  , evaluateText
  , exposeFunction
  , goto
  , isClosed
  , mouseClick
  , newPage
  , onContextPage
  , opener
  , pageContext
  , requestAllHeaders
  , requestResponse
  , responseFinished
  , route
  , setContent
  , undefinedArgument
  , urlFromText
  , waitForClose
  , waitForDialog
  , waitForDownload
  , waitForPopup
  , waitForRequest
  , waitForResponse
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssets
  , withChromiumContext
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
  )
import System.Timeout
  ( timeout
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( concurrently_
  , wait
  , withAsync
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-close"
    [ testCase "should set the page close state" $
        withChromiumPage $ \page -> do
          assertEqual "open" False =<< isClosed page
          closePage page
          assertEqual "closed" True =<< isClosed page
    , testCase "should pass page to close event" $
        withChromiumPage $ \page ->
          withAsync (waitForClose page) $ \handle -> do
            closePage page
            wait handle
            assertEqual "closed" True =<< isClosed page
    , testCase "should be callable twice" $
        withChromiumPage $ \page -> do
          closePage page
          closePage page
    , testCase "should reject all promises when page is closed" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Target page, context or browser has been closed"
            ( concurrently_
                (evaluate page "() => new Promise(() => {})")
                (closePage page)
            )
    , testCase "should return null if parent page has been closed" $
        withChromiumPage $ \page ->
          withAsync (waitForPopup page) $ \handle -> do
            _ <- evaluate page "() => window.open('about:blank')"
            popup <- wait handle
            closePage page
            matched <- opener popup
            assertEqual "opener" True (isNothing matched)
    , testCase "Page.bringToFront should work" $
        withChromiumContext $ \ctx page1 -> do
          page2 <- newPage ctx
          setContent page1 "Page1"
          setContent page2 "Page2"
          bringToFront page1
          assertEqual "page1" "visible" =<< evaluateText page1 "() => document.visibilityState"
          assertEqual "page2" "visible" =<< evaluateText page2 "() => document.visibilityState"
          bringToFront page2
          assertEqual "page1 after" "visible" =<< evaluateText page1 "() => document.visibilityState"
          assertEqual "page2 after" "visible" =<< evaluateText page2 "() => document.visibilityState"
    , testCase "should close page with active dialog" $
        withChromiumPage $ \page -> do
          setContent page "<button onclick=\"setTimeout(() => alert(1))\">alert</button>"
          withAsync (clickButton page) $ \_ -> do
            _ <- waitForDialog page
            closePage page
    , testCase "should terminate network waiters" $
        withAssets $ \prefix page -> do
          let
            empty = UrlGlob (prefix <> "/empty.html")
          withAsync (waitForRequest page empty) $ \reqHandle ->
            withAsync (waitForResponse page empty) $ \respHandle -> do
              closePage page
              assertClosedNotTimeout (wait reqHandle)
              assertClosedNotTimeout (wait respHandle)
    , testCase "should fail with error upon disconnect" $
        withChromiumPage $ \page ->
          assertClosedNotTimeout
            ( concurrently_
                (void (waitForDownload page))
                (closePage page)
            )
    , testCase "should not throw UnhandledPromiseRejection when page closes" $
        withChromiumPage $ \page -> do
          _ <-
            try
              ( concurrently_
                  (closePage page)
                  (mouseClick page (Point (cssPixelFromInt 1) (cssPixelFromInt 2)))
              ) ::
              Playwright (Either PlaywrightError ())
          pure ()
    , testCase "interrupt request.response() and request.allHeaders() on page.close" $
        withChromiumPage $ \page ->
          withLocalApp hangingStyleApp $ \prefix -> do
            withAsync (waitForRequest page (UrlGlob "**/one-style.css")) $ \pending -> do
              _ <-
                goto
                  page
                  (urlFromText (prefix <> "/one-style.html"))
                  defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateDomContentLoaded}
              request <- wait pending
              withAsync (requestResponse request) $ \respHandle ->
                withAsync (requestAllHeaders request) $ \headersHandle -> do
                  closePage page
                  assertClosedNotTimeout (wait respHandle)
                  assertClosedNotTimeout (wait headersHandle)
    , testCase "should not treat navigations as new popups" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page ("<a target=_blank rel=noopener href=\"" <> prefix <> "/one-style.html\">yo</a>")
          withAsync (waitForPopup page) $ \pending -> do
            clickButtonLink page
            popup <- wait pending
            extra <- newIORef False
            onContextPage (pageContext page) $ \_ -> writeIORef extra True
            _ <- goto popup (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            closePage page
            sawExtra <- readIORef extra
            assertEqual "second popup" False sawExtra
    , testCase "should not result in unhandled rejection" $
        withChromiumPage $ \page ->
          withAsync (waitForClose page) $ \pending -> do
            _ <-
              exposeFunction page (BindingName "foo") $ \_ -> do
                closePage page
                pure undefinedArgument
            _ <- evaluate page "() => { window.setTimeout(() => window.foo(), 0); return undefined; }"
            wait pending
            assertProtocolContains
              "Target page, context or browser has been closed"
              (evaluate page "1 + 1")
    , testCase "should reject response.finished if page closes" $
        withChromiumPage $ \page ->
          withLocalApp hangingGetApp $ \prefix -> do
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            withAsync (waitForResponse page (UrlGlob "**/get")) $ \pending -> do
              _ <- evaluate page "() => fetch('./get', { method: 'GET' })"
              response <- wait pending
              withAsync (responseFinished response) $ \finishHandle -> do
                closePage page
                result <- try (wait finishHandle)
                case result of
                  Left (ProtocolRpc failure) ->
                    assertBool "closed" ("closed" `isInfixOf` protocolFailureMessage failure)
                  Left err ->
                    assertFailure ("expected ProtocolRpc, got: " <> show err)
                  Right () ->
                    assertFailure "expected response.finished to fail"
    , testCase "should not throw when continuing while page is closing" $
        withAssets $ \prefix page -> do
          done <- newEmptyMVar
          route page (UrlGlob "**/*") $ \handled -> do
            concurrently_ (continueRoute handled) (closePage page)
            putMVar done ()
          _ <-
            try (void (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)) ::
              Playwright (Either PlaywrightError ())
          takeMVar done
    , testCase "should not throw when continuing after page is closed" $
        withAssets $ \prefix page -> do
          done <- newEmptyMVar
          route page (UrlGlob "**/*") $ \handled -> do
            closePage page
            continueRoute handled
            putMVar done ()
          result <-
            try (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions) ::
              Playwright (Either PlaywrightError (Maybe Response))
          takeMVar done
          case result of
            Left _ ->
              pure ()
            Right _ ->
              assertFailure "expected goto to fail"
    ]

clickButton :: Page -> Playwright ()
clickButton page =
  click (locate page "button")

clickButtonLink :: Page -> Playwright ()
clickButtonLink page =
  click (locate page "a")

assertClosedNotTimeout :: Playwright a -> Playwright ()
assertClosedNotTimeout action = do
  result <- try action
  case result of
    Left (ProtocolRpc failure) -> do
      assertBool
        "closed"
        ("Target page, context or browser has been closed" `isInfixOf` protocolFailureMessage failure)
      assertBool "not timeout" (not ("Timeout" `isInfixOf` protocolFailureMessage failure))
    Left err ->
      assertFailure ("expected ProtocolRpc, got: " <> show err)
    Right _ ->
      assertFailure "expected target closed error"

hangingStyleApp :: Application
hangingStyleApp request respond =
  case pathInfo request of
    ["one-style.html"] ->
      respond $
        responseLBS
          status200
          [(hContentType, "text/html")]
          (LBS8.pack "<link rel=\"stylesheet\" href=\"one-style.css\"><div>hello</div>")
    ["one-style.css"] ->
      respond $
        responseStream
          status200
          [(hContentType, "text/css")]
          $ \_write _flush ->
            void (timeout 5000000 (forever (threadDelay 100000)))
    _ ->
      respond $
        responseLBS status200 [(hContentType, "text/html")] (LBS8.pack "")

hangingGetApp :: Application
hangingGetApp request respond =
  case pathInfo request of
    ["get"] ->
      respond $
        responseStream
          status200
          [(hContentType, "text/plain; charset=utf-8")]
          $ \write flush -> do
            write (byteString (BS8.pack "hello "))
            flush
            void (timeout 5000000 (forever (threadDelay 100000)))
    _ ->
      respond $
        responseLBS status200 [(hContentType, "text/html")] (LBS8.pack "")

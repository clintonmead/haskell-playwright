module Playwright.BrowserContextPageEventSpec
  ( tests
  ) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
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
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  )
import Playwright
  ( ClickOptions (clickOptionsModifiers)
  , KeyboardModifier (KeyboardModifierControlOrMeta, KeyboardModifierShift)
  , LoadState (LoadStateDomContentLoaded)
  , Playwright
  , armContextPage
  , clickWith
  , closePage
  , contextPages
  , defaultClickOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluate
  , evaluateText
  , evaluateWith
  , goto
  , newContext
  , newPage
  , onContextPage
  , onPageClose
  , opener
  , pagesEqual
  , querySelector
  , selectorFromText
  , setContent
  , textArgument
  , url
  , urlFromText
  , urlToText
  , waitForLoadState
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , locate
  , recordingPageApp
  , testAssetsDir
  , withAssetsContextWith
  , withChromiumBrowser
  , withChromiumContext
  , withLocalApp
  )
import UnliftIO.IORef
  ( IORef
  , writeIORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( Text
  )
import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( isNothing
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
    "browsercontext-page-event"
    [ testCase "should have url" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page ->
          armContextPage ctx >>= \pending -> do
            _ <-
              evaluateWith
                page
                "url => window.open(url)"
                (textArgument (prefix <> "/empty.html"))
            other <- pending
            assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url other
    , testCase "should have url after domcontentloaded" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page ->
          armContextPage ctx >>= \pending -> do
            _ <-
              evaluateWith
                page
                "url => window.open(url)"
                (textArgument (prefix <> "/empty.html"))
            other <- pending
            waitForLoadState other LoadStateDomContentLoaded
            assertEqual "url" (prefix <> "/empty.html") . urlToText =<< url other
    , testCase "should have about:blank url with domcontentloaded" $
        withChromiumContext $ \ctx page ->
          armContextPage ctx >>= \pending -> do
            _ <- evaluateWith page "url => window.open(url)" (textArgument "about:blank")
            other <- pending
            waitForLoadState other LoadStateDomContentLoaded
            assertEqual "url" "about:blank" . urlToText =<< url other
    , testCase "should have about:blank for empty url with domcontentloaded" $
        withChromiumContext $ \ctx page ->
          armContextPage ctx >>= \pending -> do
            _ <- evaluate page "() => window.open()"
            other <- pending
            waitForLoadState other LoadStateDomContentLoaded
            assertEqual "url" "about:blank" . urlToText =<< url other
    , testCase "should report when a new page is created and closed" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page ->
          armContextPage ctx >>= \pending -> do
            _ <-
              evaluateWith
                page
                "url => window.open(url)"
                (textArgument (prefix <> "/empty.html"))
            other <- pending
            assertEqual "hello" "Hello world" =<< evaluateText other "() => ['Hello', 'world'].join(' ')"
            body <- querySelector other (selectorFromText "body")
            case body of
              Nothing ->
                assertFailure "expected body"
              Just _ ->
                pure ()
            listed <- contextPages ctx
            assertEqual "has original" True (any (pagesEqual page) listed)
            assertEqual "has other" True (any (pagesEqual other) listed)
            closePage other
            after <- contextPages ctx
            assertEqual "other gone" False (any (pagesEqual other) after)
            assertEqual "original remains" True (any (pagesEqual page) after)
    , testCase "should report initialized pages" $
        withChromiumContext $ \ctx _ ->
          armContextPage ctx >>= \pending -> do
            created <- newPage ctx
            waited <- pending
            assertEqual "same page" True (pagesEqual created waited)
            assertEqual "blank" "about:blank" . urlToText =<< url waited
            armContextPage ctx >>= \popupPending -> do
              _ <- evaluate created "() => window.open('about:blank')"
              popup <- popupPending
              assertEqual "popup blank" "about:blank" . urlToText =<< url popup
    , testCase "should have an opener" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          armContextPage ctx >>= \pending -> do
            _ <- goto page (urlFromText (prefix <> "/popup/window-open.html")) defaultGotoOptions
            popup <- pending
            assertEqual "popup url" (prefix <> "/popup/popup.html") . urlToText =<< url popup
            matched <- opener popup
            case matched of
              Nothing ->
                assertFailure "expected opener"
              Just parent ->
                assertEqual "opener identity" True (pagesEqual parent page)
            none <- opener page
            case none of
              Nothing ->
                pure ()
              Just _ ->
                assertFailure "expected no opener on original page"
    , testCase "should work with Shift-clicking" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a href=\"/one-style.html\">yo</a>"
          takePopup <- armContextPage ctx
          clickWith (locate page "a") defaultClickOptions {clickOptionsModifiers = Just [KeyboardModifierShift]}
          popup <- takePopup
          assertBool "no opener" . isNothing =<< opener popup
    , testCase "should work with Ctrl-clicking" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a href=\"/one-style.html\">yo</a>"
          takePopup <- armContextPage ctx
          clickWith (locate page "a") defaultClickOptions {clickOptionsModifiers = Just [KeyboardModifierControlOrMeta]}
          popup <- takePopup
          assertBool "no opener" . isNothing =<< opener popup
    , testCase "should fire page lifecycle events" $ do
        requests <- newIORef []
        withChromiumContext $ \ctx _ ->
          withLocalApp (recordingPageApp requests) $ \prefix -> do
            events <- newIORef []
            -- The close listener reads the URL recorded after goto, because asking
            -- a page that is closing for its URL can fail and drop the event.
            lastUrl <- newIORef ""
            onContextPage ctx $ \created -> do
              createdUrl <- urlToText <$> url created
              modifyIORef' events (<> ["CREATED: " <> createdUrl])
              onPageClose created $ do
                closedUrl <- readIORef lastUrl
                modifyIORef' events (<> ["DESTROYED: " <> closedUrl])
            page <- newPage ctx
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            writeIORef lastUrl =<< urlToText <$> url page
            closePage page
            assertEqual "events" ["CREATED: about:blank", "DESTROYED: " <> prefix <> "/empty.html"]
              =<< awaitEvents events ["CREATED: about:blank", "DESTROYED: " <> prefix <> "/empty.html"] 200
    , testCase "should not crash while redirecting of original request was missed" $ do
        assets <- testAssetsDir
        arrival <- newEmptyMVar
        release <- newEmptyMVar
        withChromiumBrowser $ \browser ->
          withLocalApp (holdStylesheetApp assets arrival release) $ \prefix -> do
            ctx <- newContext browser
            page <- newPage ctx
            _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
            takeNewPage <- armContextPage ctx
            _ <- evaluateText page ("() => { window.open('" <> prefix <> "/one-style.html'); return 'opened'; }")
            takeMVar arrival
            newPage' <- takeNewPage
            putMVar release ()
            _ <- waitForLoadState newPage' LoadStateDomContentLoaded
            assertEqual "url" (prefix <> "/one-style.html") . urlToText =<< url newPage'
    ]

-- | Polls the recorded events until they match the expected list, or the
-- attempts run out. A close listener runs after closePage returns, so the
-- events are read after a short wait.
awaitEvents :: IORef [Text] -> [Text] -> Int -> Playwright [Text]
awaitEvents events expected attempts = do
  current <- readIORef events
  if current == expected || attempts <= 0
    then pure current
    else do
      liftIO (threadDelay 50000)
      awaitEvents events expected (attempts - 1)

-- | Serves the assets, except that /one-style.css reports its arrival and then
-- answers with a redirect once released.
holdStylesheetApp :: FilePath -> MVar () -> MVar () -> Application
holdStylesheetApp assets arrival release request respond =
  case pathInfo request of
    ["one-style.css"] -> do
      putMVar arrival ()
      takeMVar release
      respond (responseLBS status302 [(hLocation, "/injectedstyle.css")] LBS8.empty)
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

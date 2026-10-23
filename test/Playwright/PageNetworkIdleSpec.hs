module Playwright.PageNetworkIdleSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString.Builder
  ( Builder
  , byteString
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import GHC.Clock
  ( getMonotonicTimeNSec
  )
import Network.HTTP.Types
  ( status200
  , status404
  )
import Network.Wai
  ( Application
  , ResponseReceived
  , pathInfo
  , responseLBS
  , responseStream
  )
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Playwright
  ( ContentType (ContentType)
  , FulfillBody (FulfillText)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , Frame
  , GotoOptions (gotoOptionsWaitUntil)
  , HttpStatus (HttpStatus)
  , LoadState (LoadStateNetworkIdle)
  , Page
  , Playwright
  , Response
  , SetContentOptions (setContentOptionsWaitUntil)
  , UrlMatch (UrlMatchGlob)
  , WaitForNavigationOptions (waitForNavigationOptionsWaitUntil)
  , armNavigationOptions
  , armPopup
  , childFrames
  , click
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultSetContentOptions
  , defaultWaitForNavigationOptions
  , evaluate
  , fulfillRoute
  , goto
  , gotoFrame
  , locator
  , mainFrame
  , responseStatus
  , route
  , selectorFromText
  , setContent
  , setContentWith
  , urlFromText
  , waitForLoadState
  , waitForTimeout
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , assertStillPending
  , requireTimeout
  , testAssetsDir
  , withChromiumPage
  , withLocalApp
  )
import UnliftIO.Async
  ( race
  , wait
  , withAsync
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , takeMVar
  , tryPutMVar
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
    "page-network-idle"
    [ testCase "should navigate to empty page with networkidle" $
        withChromiumPage $ \page -> withIdleApp $ \prefix _ -> do
          response <-
            goto
              page
              (urlFromText (prefix <> "/empty.html"))
              defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateNetworkIdle}
          assertNavigationOk response
    , testCase "should wait for networkidle to succeed navigation" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          response <-
            driveNetworkIdle page hold $
              goto page (urlFromText (prefix <> "/networkidle.html")) networkIdleGoto
          assertNavigationOk response
    , testCase "should wait for networkidle to succeed navigation with request from previous navigation" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- setContent page "<script>fetch('foo.js');</script>"
          awaitSignal (holdRequestedFoo hold)
          response <-
            driveNetworkIdle page hold $
              goto page (urlFromText (prefix <> "/networkidle.html")) networkIdleGoto
          putMVar (holdReleaseFoo hold) ()
          assertNavigationOk response
    , testCase "should wait for networkidle in waitForNavigation" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          response <-
            driveNetworkIdle page hold $ do
              takeNavigation <-
                armNavigationOptions
                  page
                  defaultWaitForNavigationOptions {waitForNavigationOptionsWaitUntil = Just LoadStateNetworkIdle}
              _ <- goto page (urlFromText (prefix <> "/networkidle.html")) defaultGotoOptions
              takeNavigation
          assertNavigationOk response
    , testCase "should wait for networkidle in setContent" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          driveNetworkIdle page hold $
            setContentWith page "<script src='networkidle.js'></script>" networkIdleSetContent
    , testCase "should wait for networkidle in setContent with request from previous navigation" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- setContent page "<script>fetch('foo.js');</script>"
          awaitSignal (holdRequestedFoo hold)
          driveNetworkIdle page hold $
            setContentWith page "<script src='networkidle.js'></script>" networkIdleSetContent
          putMVar (holdReleaseFoo hold) ()
    , testCase "should wait for networkidle when navigating iframe" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          frame <- firstChildFrame page
          response <-
            driveNetworkIdle page hold $
              gotoFrame frame (urlFromText (prefix <> "/networkidle.html")) networkIdleGoto
          assertNavigationOk response
    , testCase "should wait for networkidle in setContent from the child frame" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          driveNetworkIdle page hold $
            setContentWith page "<iframe src='networkidle.html'></iframe>" networkIdleSetContent
    , testCase "should wait for networkidle from the child frame" $
        withChromiumPage $ \page -> withIdleApp $ \prefix hold -> do
          response <-
            driveNetworkIdle page hold $
              goto page (urlFromText (prefix <> "/networkidle-frame.html")) networkIdleGoto
          assertNavigationOk response
    , testCase "should wait for networkidle from the popup" $
        withChromiumPage $ \page -> withIdleApp $ \prefix _ -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- setContent page popupButtons
          mapM_ (openPopupIdle page) [1 .. 5 :: Int]
    , testCase "should wait for networkidle when iframe attaches and detaches" $
        withChromiumPage $ \page -> withIdleApp $ \prefix _ -> do
          route page (UrlMatchGlob "**/empty.html") (\_ -> pure ())
          withAsync (setContentWith page (attachIframe prefix) networkIdleSetContent) $ \pending -> do
            timeout <- requireTimeout 600
            waitForTimeout page timeout
            assertStillPending pending
            _ <- evaluate page "() => document.querySelector('iframe').remove()"
            wait pending
    , testCase "should work after repeated navigations in the same page" $
        withChromiumPage $ \page -> withIdleApp $ \prefix _ -> do
          sampleCount <- liftIO (newIORef (0 :: Int))
          route page (UrlMatchGlob "**/empty.html") $ \handled ->
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "text/html")
                , fulfillBody = Just (FulfillText fetchSampleHtml)
                }
          route page (UrlMatchGlob "**/sample") $ \handled -> do
            liftIO (modifyIORef' sampleCount (+ 1))
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillContentType = Just (ContentType "application/json")
                , fulfillBody = Just (FulfillText "{\"content\":\"sample\"}")
                }
          _ <- goto page (urlFromText (prefix <> "/empty.html")) networkIdleGoto
          assertEqual "sample requests after the first navigation" 1 =<< liftIO (readIORef sampleCount)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) networkIdleGoto
          assertEqual "sample requests after the second navigation" 2 =<< liftIO (readIORef sampleCount)
    , testCase "should not wait for an open EventSource connection" $
        withChromiumPage $ \page -> withIdleApp $ \prefix _ -> do
          response <-
            goto page (urlFromText (prefix <> "/sse-page.html")) networkIdleGoto
          assertNavigationOk response
    , testCase "should not wait for an open EventSource connection in setContent" $
        withChromiumPage $ \page -> withIdleApp $ \prefix _ -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- setContentWith page eventSourceScript networkIdleSetContent
          _ <- evaluate page "() => window.__sseOpened"
          pure ()
    ]

-- | The goto option that waits for the network to go idle.
networkIdleGoto :: GotoOptions
networkIdleGoto = defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateNetworkIdle}

-- | The setContent option that waits for the network to go idle.
networkIdleSetContent :: SetContentOptions
networkIdleSetContent = defaultSetContentOptions {setContentOptionsWaitUntil = Just LoadStateNetworkIdle}

-- | Requests the test holds open, one set per round, and the signals that
-- say when each one has arrived and when it may be answered.
data IdleHold = IdleHold
  { holdRequestedA :: MVar ()
  , holdReleaseA :: MVar ()
  , holdRequestedB :: MVar ()
  , holdReleaseB :: MVar ()
  , holdRequestedFoo :: MVar ()
  , holdReleaseFoo :: MVar ()
  }

newIdleHold :: Playwright IdleHold
newIdleHold =
  IdleHold
    <$> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar
    <*> newEmptyMVar

-- | Serves the networkidle page and its scripts with the held requests.
withIdleApp :: (Text -> IdleHold -> Playwright a) -> Playwright a
withIdleApp action = do
  assets <- testAssetsDir
  hold <- newIdleHold
  withLocalApp (idleApp assets hold) (\prefix -> action prefix hold)

-- | Runs an action that should go idle, and holds its requests open. The
-- action must still be pending while the requests are open. The first request
-- is answered before the second one is held, and the action completes at least
-- 500 ms after the second request is answered.
driveNetworkIdle :: Page -> IdleHold -> Playwright a -> Playwright a
driveNetworkIdle page hold action =
  withAsync action $ \pending -> do
    awaitSignal (holdRequestedA hold)
    assertStillPending pending
    putMVar (holdReleaseA hold) ()
    _ <- evaluate page "() => window.fetchSecond()"
    awaitSignal (holdRequestedB hold)
    assertStillPending pending
    released <- liftIO getMonotonicTimeNSec
    putMVar (holdReleaseB hold) ()
    result <- wait pending
    finished <- liftIO getMonotonicTimeNSec
    assertBool "networkidle waited 500 ms after the last request" (finished - released >= 500000000)
    pure result

-- | Checks that a navigation response exists and returned status 200.
assertNavigationOk :: Maybe Response -> Playwright ()
assertNavigationOk response =
  case response of
    Nothing ->
      assertFailure "expected navigation response"
    Just value ->
      assertEqual "status" (HttpStatus 200) =<< responseStatus value

-- | Waits for a held request to arrive. A request that never arrives fails the
-- test instead of hanging the run.
awaitSignal :: MVar () -> Playwright ()
awaitSignal signal = do
  outcome <- race (liftIO (threadDelay 30000000)) (takeMVar signal)
  case outcome of
    Left () ->
      assertFailure "expected request never arrived"
    Right () ->
      pure ()

-- | The first child frame of the main frame of the page.
firstChildFrame :: Page -> Playwright Frame
firstChildFrame page = do
  frames <- childFrames (mainFrame page)
  case frames of
    frame : _ ->
      pure frame
    [] ->
      assertFailure "expected a child frame"

-- | Clicks the button that opens the popup, and waits for the popup to go idle.
openPopupIdle :: Page -> Int -> Playwright ()
openPopupIdle page index = do
  takePopup <- armPopup page
  _ <- click (locator page (selectorFromText ("#box" <> Text.pack (show index))))
  popup <- takePopup
  waitForLoadState popup LoadStateNetworkIdle

-- | Five buttons, each of which opens the popup page.
popupButtons :: Text
popupButtons =
  Text.unlines (map popupButton [1 .. 5 :: Int])
  where
    popupButton :: Int -> Text
    popupButton index =
      "<button id=box"
        <> Text.pack (show index)
        <> " onclick=\"window.open('./popup/popup.html')\">Button"
        <> Text.pack (show index)
        <> "</button>"

-- | Appends an iframe to the document, which loads the empty page.
attachIframe :: Text -> Text
attachIframe prefix =
  "<body><script>const iframe = document.createElement('iframe'); iframe.src = '"
    <> prefix
    <> "/empty.html'; document.body.appendChild(iframe);</script></body>"

-- | The empty page replacement that fetches the sample endpoint from the page's own origin.
fetchSampleHtml :: Text
fetchSampleHtml =
  "<script>fetch('/sample').then(res => res.json()).then(() => {});</script>"

-- | Opens an event stream that stays open, and resolves once the first message arrives.
eventSourceScript :: Text
eventSourceScript =
  "<script>window.__sseOpened = new Promise(resolve => { const es = new EventSource('/sse'); es.onmessage = () => resolve(); });</script>"

-- | Serves the assets. The two fetch requests and the request from a previous
-- navigation are held until the test releases them, and each one signals that
-- it arrived. The event stream sends one message and then stays open.
idleApp :: FilePath -> IdleHold -> Application
idleApp assets hold request respond =
  case pathInfo request of
    ["fetch-request-a.js"] ->
      held (holdRequestedA hold) (holdReleaseA hold)
    ["fetch-request-b.js"] ->
      held (holdRequestedB hold) (holdReleaseB hold)
    ["foo.js"] ->
      held (holdRequestedFoo hold) (holdReleaseFoo hold)
    ["sse"] ->
      respond
        ( responseStream
            status200
            [("Content-Type", "text/event-stream"), ("Cache-Control", "no-cache")]
            eventStream
        )
    ["sse-page.html"] ->
      respond
        ( responseLBS
            status200
            [("Content-Type", "text/html")]
            (LBS8.pack "<script>new EventSource('/sse');</script>")
        )
    _ ->
      staticApp (defaultFileServerSettings assets) request respond
  where
    held :: MVar () -> MVar () -> IO ResponseReceived
    held requested release = do
      _ <- tryPutMVar requested ()
      _ <- takeMVar release
      respond (responseLBS status404 [] (LBS8.pack "File not found"))

-- | Sends one event and then keeps the stream open for longer than a test runs.
eventStream :: (Builder -> IO ()) -> IO () -> IO ()
eventStream write flush = do
  write (byteString "data: hello\n\n")
  flush
  threadDelay 60000000

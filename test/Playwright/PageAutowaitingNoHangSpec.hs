module Playwright.PageAutowaitingNoHangSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( MonadIO
  )
import UnliftIO.Concurrent
  ( threadDelay
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )
import UnliftIO.Exception
  ( SomeException
  , throwIO
  , try
  )
import Control.Monad
  ( forever
  , void
  )
import Data.ByteString.Lazy qualified as LBS
import Data.Text
  ( Text
  , isInfixOf
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  , status404
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
import Playwright
  ( Page
  , Response
  , Timeout
  , click
  , defaultGotoOptions
  , evaluate
  , evaluateWith
  , goBack
  , goto
  , innerText
  , milliseconds
  , setContent
  , textArgument
  , url
  , urlFromText
  , urlToText
  , waitForPopup
  , waitForTimeout
  , Playwright
  )
import Playwright.Harness
  ( locate
  , testAssetsDir
  , withChromiumPage
  , withLocalApp
  , assertEqual
  , assertFailure
  )
import System.IO.Error
  ( mkIOError
  )
import GHC.IO.Exception
  ( IOErrorType (ResourceVanished)
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
    "page-autowaiting-no-hang"
    [ testCase "calling window.stop async" $
        withHangEmpty $ \prefix page -> do
          _ <-
            evaluateWith
              page
              "url => { window.location.href = url; setTimeout(() => window.stop(), 100); }"
              (textArgument (prefix <> "/empty.html"))
          pure ()
    , testCase "calling window.stop sync" $
        withHangEmpty $ \prefix page -> do
          _ <-
            evaluateWith
              page
              "url => { window.location.href = url; window.stop(); }"
              (textArgument (prefix <> "/empty.html"))
          pure ()
    , testCase "assigning location to about:blank" $
        withEmptyPage $ \page -> do
          _ <- evaluate page "() => { window.location.href = \"about:blank\"; }"
          pure ()
    , testCase "assigning location to about:blank after non-about:blank" $
        withHangEmpty $ \prefix page -> do
          _ <-
            evaluate
              page
              ( "() => { window.location.href = \""
                  <> prefix
                  <> "/empty.html\"; window.location.href = \"about:blank\"; }"
              )
          pure ()
    , testCase "calling window.open and window.close" $
        withEmptyPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { const popup = window.open(window.location.href); popup.close(); }"
          pure ()
    , testCase "opening a popup" $
        withEmptyPage $ \page ->
          withAsync (waitForPopup page) $ \handle -> do
            _ <- evaluate page "() => window.open(window.location.href) && 1"
            void (wait handle)
    , testCase "clicking on links which do not commit navigation" $
        withPageApp emptyOkApp $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          -- Upstream links to an https fixture; this client has no TLS
          -- fixture. Port 1 has no listener, so the navigation fails with
          -- a connection refusal and never commits.
          setContent page "<a href='https://127.0.0.1:1/empty.html'>foobar</a>"
          click (locate page "a")
    , testCase "clicking in the middle of navigation that aborts" $
        clickDuringStall DestroyConnection
    , testCase "clicking in the middle of navigation that commits" $
        clickDuringStall CommitHello
    , testCase "clicking a link intercepted by the Navigation API same-document" $
        navigationApiClick
    , testCase "goBack in the middle of navigation that commits" $
        goBackDuringCommit
    ]

data StallFinish
  = DestroyConnection
  | CommitHello

clickDuringStall :: StallFinish -> IO ()
clickDuringStall finish = do
  stall <- newEmptyMVar
  release <- newEmptyMVar
  timeout <- requireMs 1000
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["stall.html"] -> do
          putMVar stall ()
          takeMVar release
          case finish of
            DestroyConnection ->
              throwIO (mkIOError ResourceVanished "destroyed" Nothing Nothing)
            CommitHello ->
              respond
                ( responseLBS
                    status200
                    [(hContentType, "text/html")]
                    "hello world"
                )
        _ ->
          static request respond
  withPageApp app $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
    withAsync (void (tryGoto page (prefix <> "/stall.html"))) $ \_ -> do
      takeMVar stall
      withAsync (click (locate page "body")) $ \clicked -> do
        waitForTimeout page timeout
        putMVar release ()
        wait clicked
        case finish of
          DestroyConnection ->
            pure ()
          CommitHello ->
            assertEqual "body" True . ("hello world" `isInfixOf`) =<< innerText (locate page "body")

goBackDuringCommit :: IO ()
goBackDuringCommit = do
  stall <- newEmptyMVar
  release <- newEmptyMVar
  timeout <- requireMs 1000
  assets <- testAssetsDir
  let
    static = staticApp (defaultFileServerSettings assets)
    app :: Application
    app request respond =
      case pathInfo request of
        ["stall.html"] -> do
          putMVar stall ()
          takeMVar release
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                "hello world"
            )
        _ ->
          static request respond
  withPageApp app $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
    withAsync (void (tryGoto page (prefix <> "/stall.html"))) $ \_ -> do
      takeMVar stall
      withAsync (void (tryGoBack page)) $ \backed -> do
        waitForTimeout page timeout
        putMVar release ()
        wait backed

navigationApiClick :: IO ()
navigationApiClick = do
  let
    app :: Application
    app request respond =
      case pathInfo request of
        ["intercept.html"] ->
          respond
            ( responseLBS
                status200
                [(hContentType, "text/html")]
                interceptHtml
            )
        _ ->
          respond (responseLBS status404 [] "")
  withPageApp app $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/intercept.html")) defaultGotoOptions
    assertEqual "initial" "initial" =<< innerText (locate page "#status")
    click (locate page "#go")
    assertEqual "intercepted" "intercepted:/other" =<< innerText (locate page "#status")
    assertEqual "path" True . ("/other" `isInfixOf`) . urlToText =<< url page

interceptHtml :: LBS.ByteString
interceptHtml =
  "<a id=\"go\" href=\"/other\">go</a><p id=\"status\">initial</p><script>navigation.addEventListener('navigate', event => { if (!event.canIntercept) return; event.intercept({ handler: async () => { const dest = new URL(event.destination.url).pathname; document.getElementById('status').textContent = 'intercepted:' + dest; } }); });</script>"

tryGoto :: Page -> Text -> Playwright ()
tryGoto page target = do
  result <- try (goto page (urlFromText target) defaultGotoOptions) :: Playwright (Either SomeException (Maybe Response))
  case result of
    Left _ ->
      pure ()
    Right _ ->
      pure ()

tryGoBack :: Page -> Playwright ()
tryGoBack page = do
  result <- try (goBack page) :: Playwright (Either SomeException (Maybe Response))
  case result of
    Left _ ->
      pure ()
    Right _ ->
      pure ()

withHangEmpty :: (Text -> Page -> Playwright a) -> IO a
withHangEmpty action =
  withPageApp hangEmptyApp action

hangEmptyApp :: Application
hangEmptyApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      forever (threadDelay 1000000)
    _ ->
      respond (responseLBS status404 [] "")

withEmptyPage :: (Page -> Playwright a) -> IO a
withEmptyPage action =
  withPageApp emptyOkApp $ \prefix page -> do
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    action page

emptyOkApp :: Application
emptyOkApp _request respond =
  respond (responseLBS status200 [(hContentType, "text/html")] "")

withPageApp :: Application -> (Text -> Page -> Playwright a) -> IO a
withPageApp app action =
  withChromiumPage $ \page ->
    withLocalApp app $ \prefix ->
      action prefix page

requireMs :: (MonadIO m) => Double -> m Timeout
requireMs n =
  case milliseconds n of
    Nothing ->
      assertFailure ("timeout " <> show n)
    Just timeout ->
      pure timeout

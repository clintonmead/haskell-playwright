module Playwright.PageAutowaitingBasicSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Control.Monad
  ( forever
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( Text
  , pack
  , replace
  )
import Data.Text qualified as Text
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
import Network.Wai.Handler.Warp
  ( withApplication
  )
import Playwright
  ( ClickOptions (clickOptionsNoWaitAfter)
  , LoadState (LoadStateLoad)
  , Page
  , armLoadEvent
  , armFrameNavigated
  , click
  , clickWith
  , dblclick
  , defaultClickOptions
  , defaultGotoOptions
  , fill
  , goto
  , setContent
  , urlFromText
  , waitForLoadState
  )
import Playwright.Harness
  ( locate
  , readTestAsset
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-autowaiting-basic"
    [ testCase "should await navigation when clicking anchor" $
        awaitAnchorClick False
    , testCase "should not stall on JS navigation link" $
        withChromiumPage $ \page -> do
          setContent page "<a href=\"javascript:console.log(1)\">console.log</a>"
          click (locate page "a")
    , testCase "should await cross-process navigation when clicking anchor" $
        awaitAnchorClick True
    , testCase "should await form-get on click" $
        awaitFormClick "get"
    , testCase "should await form-post on click" $
        awaitFormClick "post"
    , testCase "should work with noWaitAfter: true" $
        hangClick True
    , testCase "should work with dblclick without noWaitAfter when navigation is stalled" $
        hangClick False
    , testCase "should work with waitForLoadState(load)" $
        clickThenLoad
    , testCase "should work with goto following click" $
        gotoAfterClick
    ]

awaitAnchorClick :: Bool -> IO ()
awaitAnchorClick cross =
  withMessagesApp $ \messages prefix page -> do
    let
      target =
        if cross
          then replace "127.0.0.1" "localhost" prefix <> "/empty.html"
          else prefix <> "/empty.html"
    setContent page ("<a id=\"anchor\" href=\"" <> target <> "\">empty.html</a>")
    clickAndNavigate messages page

awaitFormClick :: Text -> IO ()
awaitFormClick method =
  withMessagesApp $ \messages prefix page -> do
    setContent
      page
      ( "<form action=\""
          <> prefix
          <> "/empty.html\" method=\""
          <> method
          <> "\"><input name=\"foo\" value=\"bar\"><input type=\"submit\" value=\"Submit\"></form>"
      )
    awaitNav <- armFrameNavigated page
    withAsync (click (locate page "input[type=submit]")) $ \clicked -> do
      awaitNav
      modifyIORef' messages (<> ["navigated"])
      wait clicked
      modifyIORef' messages (<> ["click"])
      assertEqual "order" "route|navigated|click" . Text.intercalate "|" =<< readIORef messages

clickAndNavigate :: IORef [Text] -> Page -> IO ()
clickAndNavigate messages page = do
  awaitNav <- armFrameNavigated page
  withAsync (click (locate page "a")) $ \clicked -> do
    awaitNav
    modifyIORef' messages (<> ["navigated"])
    wait clicked
    modifyIORef' messages (<> ["click"])
    assertEqual "order" "route|navigated|click" . Text.intercalate "|" =<< readIORef messages

hangClick :: Bool -> IO ()
hangClick useClick =
  withHangEmpty $ \prefix page -> do
    setContent page ("<a id=\"anchor\" href=\"" <> prefix <> "/empty.html\">empty.html</a>")
    if useClick
      then
        clickWith
          (locate page "a")
          defaultClickOptions {clickOptionsNoWaitAfter = Just True}
      else
        dblclick (locate page "a")

clickThenLoad :: IO ()
clickThenLoad =
  withMessagesApp $ \messages prefix page -> do
    setContent page ("<a id=\"anchor\" href=\"" <> prefix <> "/empty.html\">empty.html</a>")
    awaitLoad <- armLoadEvent page
    withAsync (click (locate page "a")) $ \clicked -> do
      awaitLoad
      modifyIORef' messages (<> ["load"])
      wait clicked
      waitForLoadState page LoadStateLoad
      modifyIORef' messages (<> ["clickload"])
      assertEqual "order" "route|load|clickload" . Text.intercalate "|" =<< readIORef messages

gotoAfterClick :: IO ()
gotoAfterClick =
  withMessagesApp $ \_ prefix page -> do
    setContent
      page
      ( "<form action=\""
          <> prefix
          <> "/login.html\" method=\"get\"><input type=\"text\"><input type=\"submit\" value=\"Submit\"></form>"
      )
    fill (locate page "input[type=text]") "admin"
    click (locate page "input[type=submit]")
    _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
    pure ()

withMessagesApp :: (IORef [Text] -> Text -> Page -> IO a) -> IO a
withMessagesApp action = do
  messages <- newIORef ([] :: [Text])
  css <- readTestAsset "one-style.css"
  withChromiumPage $ \page ->
    withApplication (pure (messagesApp messages css)) $ \port ->
      action messages ("http://127.0.0.1:" <> pack (show port)) page

withHangEmpty :: (Text -> Page -> IO a) -> IO a
withHangEmpty action =
  withChromiumPage $ \page ->
    withApplication (pure hangEmptyApp) $ \port ->
      action ("http://127.0.0.1:" <> pack (show port)) page

messagesApp :: IORef [Text] -> ByteString -> Application
messagesApp messages css request respond =
  case pathInfo request of
    ["empty.html"] -> do
      modifyIORef' messages (<> ["route"])
      respond
        ( responseLBS
            status200
            [(hContentType, "text/html")]
            "<link rel='stylesheet' href='./one-style.css'>"
        )
    ["one-style.css"] ->
      respond (responseLBS status200 [(hContentType, "text/css")] (LBS.fromStrict css))
    ["login.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "You are logged in")
    _ ->
      respond (responseLBS status404 [] "")

hangEmptyApp :: Application
hangEmptyApp request respond =
  case pathInfo request of
    ["empty.html"] ->
      forever (threadDelay 1000000)
    _ ->
      respond (responseLBS status404 [] "")

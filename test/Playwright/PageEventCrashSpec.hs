module Playwright.PageEventCrashSpec
  ( tests
  ) where

import Control.Monad
  ( void
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
  , responseLBS
  )
import Playwright
  ( Exactness (Inexact)
  , LoadState (LoadStateDomContentLoaded)
  , Page
  , Playwright
  , Response
  , UrlMatch (UrlMatchGlob)
  , WaitForNavigationOptions (waitForNavigationOptionsWaitUntil)
  , WaitForState (WaitForVisible)
  , closeContext
  , closePage
  , defaultGotoOptions
  , defaultWaitForNavigationOptions
  , evaluate
  , getByText
  , goto
  , isClosed
  , milliseconds
  , route
  , setContent
  , setDefaultTimeout
  , urlFromText
  , waitFor
  , waitForCrash
  , waitForNavigationOptions
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( withChromiumContext
  , withChromiumPage
  , assertBool
  , assertEqual
  , assertFailure
  , withLocalApp
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO
  ( try
  )
import UnliftIO.Async
  ( async
  , wait
  , withAsync
  )

tests :: TestTree
tests =
  testGroup
    "page-event-crash"
    [ testCase "should emit crash event when page crashes" $
        withChromiumPage $ \page -> do
          setCrashTimeout page
          setContent page "<div>This page should crash</div>"
          _ <-
            async $ do
              _ <- try (goto page (urlFromText "chrome://crash") defaultGotoOptions) :: Playwright (Either PlaywrightError (Maybe Response))
              pure ()
          waitForCrash page
    , testCase "should throw on any action after page crashes" $
        withChromiumPage $ \page -> do
          setCrashTimeout page
          setContent page "<div>This page should crash</div>"
          _ <-
            async $ do
              _ <- try (goto page (urlFromText "chrome://crash") defaultGotoOptions) :: Playwright (Either PlaywrightError (Maybe Response))
              pure ()
          waitForCrash page
          result <- try (evaluate page "() => {}")
          case result of
            Left (ProtocolRpc failure) ->
              assertBool "crashed" ("crashed" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected evaluate to fail after crash"
    , testCase "should be able to close context when page crashes" $
        withChromiumContext $ \ctx page -> do
          setCrashTimeout page
          setContent page "<div>This page should crash</div>"
          _ <-
            async $ do
              _ <- try (goto page (urlFromText "chrome://crash") defaultGotoOptions) :: Playwright (Either PlaywrightError (Maybe Response))
              pure ()
          waitForCrash page
          closeContext ctx
    , testCase "expect should not hang when page crashed" $
        withChromiumPage $ \page -> do
          setCrashTimeout page
          withAsync (tryPlaywright (waitFor (getByText page "child" Inexact) WaitForVisible)) $ \pending -> do
            triggerCrash page
            result <- wait pending
            case result of
              Left _ ->
                pure ()
              Right () ->
                assertFailure "expected the visibility wait to reject after crash"
    , testCase "should cancel navigation when page crashes" $
        withChromiumPage $ \page ->
          withLocalApp navigationApp $ \prefix -> do
            setCrashTimeout page
            setContent page "<div>This page should crash</div>"
            route page (UrlMatchGlob "**/one-style.css") (\_ -> pure ())
            withAsync (tryPlaywright (goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions)) $ \pending -> do
              _ <-
                waitForNavigationOptions
                page
                defaultWaitForNavigationOptions
                  { waitForNavigationOptionsWaitUntil = Just LoadStateDomContentLoaded
                  }
              triggerCrash page
              result <- wait pending
              case result of
                Left (ProtocolRpc failure) ->
                  assertBool "Page crashed" ("Page crashed" `isInfixOf` protocolFailureMessage failure)
                Left err ->
                  assertFailure ("expected ProtocolRpc, got: " <> show err)
                Right _ ->
                  assertFailure "expected goto to fail after crash"
    , testCase "should be able to close page after crash" $
        withChromiumPage $ \page -> do
          setCrashTimeout page
          setContent page "<div>This page should crash</div>"
          triggerCrash page
          waitForCrash page
          closePage page
          closed <- isClosed page
          assertEqual "closed" True closed
    ]

-- Upstream cases not ported here:
--   should cancel waitForEvent when page crashes: upstream waits on the generic
--     page.waitForEvent('response'), which this library does not expose. The closest
--     port, waitForResponse, does not reject when the page crashes, so it times out.

setCrashTimeout :: Page -> Playwright ()
setCrashTimeout page =
  case milliseconds 5000 of
    Nothing ->
      assertFailure "timeout"
    Just timeout ->
      setDefaultTimeout page timeout

-- | Starts a navigation to chrome://crash without waiting for it, as upstream's crash fixture does.
triggerCrash :: Page -> Playwright ()
triggerCrash page =
  void $
    async $
      void (tryPlaywright (goto page (urlFromText "chrome://crash") defaultGotoOptions))

tryPlaywright :: Playwright a -> Playwright (Either PlaywrightError a)
tryPlaywright = try

-- | Serves a page whose stylesheet is held by a route, so navigation stays in flight.
navigationApp :: Application
navigationApp _request respond =
  respond (responseLBS status200 [(hContentType, "text/html")] "<link rel=\"stylesheet\" href=\"/one-style.css\"><div>one style</div>")

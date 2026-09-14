module Playwright.PageEventCrashSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( Page
  , Playwright
  , Response
  , closeContext
  , defaultGotoOptions
  , evaluate
  , goto
  , milliseconds
  , setContent
  , setDefaultTimeout
  , urlFromText
  , waitForCrash
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( withChromiumContext
  , withChromiumPage
  , assertBool
  , assertFailure
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
    ]

setCrashTimeout :: Page -> Playwright ()
setCrashTimeout page =
  case milliseconds 5000 of
    Nothing ->
      assertFailure "timeout"
    Just timeout ->
      setDefaultTimeout page timeout

module Playwright.BrowserSpec
  ( tests
  ) where

import Control.Concurrent.Async
  ( wait
  , withAsync
  )
import Control.Concurrent.STM
  ( atomically
  , newTVarIO
  , readTVar
  , readTVarIO
  , retry
  , writeTVar
  )
import Control.Exception
  ( try
  )
import Control.Monad
  ( unless
  )
import Data.Char
  ( isDigit
  )
import Data.Text
  ( Text
  , isInfixOf
  , splitOn
  )
import Data.Text qualified as Text
import Playwright
  ( BrowserContext
  , browserBrowserType
  , browserContexts
  , browserTypesEqual
  , browserVersion
  , chromium
  , closeBrowser
  , closeContext
  , closePage
  , contextsEqual
  , defaultLaunchOptions
  , evaluate
  , newBrowserPage
  , newContext
  , newPage
  , onBrowserContext
  , onPageClose
  , pageContext
  , withBrowser
  , withPlaywright
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( withChromiumBrowser
  )
import Test.Tasty
  ( TestTree
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
    "browser"
    [ testCase "should return browserType" $
        withPlaywright $ \pw ->
          withBrowser (chromium pw) defaultLaunchOptions $ \browser -> do
            got <- browserBrowserType browser
            assertEqual "same" True (browserTypesEqual got (chromium pw))
    , testCase "should create new page @smoke" $
        withChromiumBrowser $ \browser -> do
          page1 <- newBrowserPage browser
          assertEqual "one" (1 :: Int) . length =<< browserContexts browser
          page2 <- newBrowserPage browser
          assertEqual "two" (2 :: Int) . length =<< browserContexts browser
          closePage page1
          assertEqual "after first close" (1 :: Int) . length =<< browserContexts browser
          closePage page2
          assertEqual "after second close" (0 :: Int) . length =<< browserContexts browser
    , testCase "should throw upon second create new page" $
        withChromiumBrowser $ \browser -> do
          page <- newBrowserPage browser
          result <- try (newPage (pageContext page))
          closePage page
          case result of
            Left (DecodeFailure message) ->
              assertBool
                "message"
                ("Please use browser.newContext()" `isInfixOf` message)
            Left err ->
              assertFailure ("expected DecodeFailure, got: " <> show err)
            Right _ ->
              assertFailure "expected DecodeFailure"
    , testCase "version should work" $
        withChromiumBrowser $ \browser -> do
          version <- browserVersion browser
          assertBool (show version) (isChromiumVersion version)
    , testCase "should dispatch page.on(close) upon browser.close and reject evaluate" $
        withChromiumBrowser $ \browser -> do
          page <- newBrowserPage browser
          closed <- newTVarIO False
          onPageClose page (atomically (writeTVar closed True))
          withAsync (try (evaluate page "() => new Promise(() => {})")) $ \pending -> do
            closeBrowser browser
            atomically $ do
              done <- readTVar closed
              unless done retry
            result <- wait pending
            case result of
              Left (ProtocolRpc failure) ->
                assertBool
                  "closed"
                  ("Target page, context or browser has been closed" `isInfixOf` protocolFailureMessage failure)
              Left err ->
                assertFailure ("expected ProtocolRpc, got: " <> show err)
              Right _ ->
                assertFailure "expected evaluate to fail"
    , testCase "should fire context event on newContext" $
        withChromiumBrowser $ \browser -> do
          events <- newTVarIO ([] :: [BrowserContext])
          onBrowserContext browser $ \ctx ->
            atomically (writeTVar events [ctx])
          ctx <- newContext browser
          seen <- readTVarIO events
          case seen of
            [got] ->
              assertEqual "same" True (contextsEqual ctx got)
            other ->
              assertFailure ("expected one context event, got: " <> show (length other))
          closeContext ctx
    ]

-- Official `newContext should not leave a context upon failure` uses
-- `__testHookBeforeSetStorageState` / `toImpl` (Node-only).

isChromiumVersion :: Text -> Bool
isChromiumVersion version =
  case splitOn "." version of
    [a, b, c, d] ->
      numericPart a && numericPart b && numericPart c && numericPart d
    _ ->
      False

numericPart :: Text -> Bool
numericPart part =
  not (Text.null part) && Text.all isDigit part

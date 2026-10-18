module Playwright.PageWaitForFunctionSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import GHC.Clock
  ( getMonotonicTimeNSec
  )
import Playwright
  ( ConsoleMessage (consoleMessageText)
  , Frame
  , Page
  , Playwright
  , addInitScript
  , asElement
  , contentFrameHandle
  , defaultGotoOptions
  , elementArgument
  , evaluate
  , evaluateBool
  , evaluateHandle
  , evaluateOnElement
  , goto
  , jsonValueBool
  , jsonValueNumber
  , milliseconds
  , objectNumberArgument
  , onConsole
  , querySelector
  , reload
  , selectorFromText
  , setContent
  , setDefaultTimeout
  , urlFromText
  , waitForFunction
  , waitForFunctionOnFrame
  , waitForFunctionPolling
  , waitForFunctionWith
  , waitForFunctionWithArg
  , waitForTimeout
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertProtocolContains
  , requireHandle
  , withAssets
  , withChromiumPage
  , assertBool
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
import UnliftIO
  ( try
  )
import UnliftIO.Async
  ( concurrently_
  , wait
  , withAsync
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )

tests :: TestTree
tests =
  testGroup
    "page-wait-for-function"
    [ testCase "should timeout" $
        withChromiumPage $ \page -> do
          start <- liftIO getMonotonicTimeNSec
          case milliseconds 42 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              waitForTimeout page timeout
          end <- liftIO getMonotonicTimeNSec
          assertBool "waited at least half the timeout" (end - start >= 21000000)
    , testCase "should work when predicate is already true" $
        withChromiumPage $ \page -> do
          _ <- evaluateBool page "() => { window.__FOO = 1; return true; }"
          _ <- waitForFunction page "() => window.__FOO === 1"
          pure ()
    , testCase "should accept a function and wait" $
        withChromiumPage $ \page ->
          concurrently_
            (waitForFunction page "() => window.__FOO === 1")
            (evaluateBool page "() => { window.__FOO = 1; return true; }")
    , testCase "should work when resolved right before execution context disposal" $
        withChromiumPage $ \page -> do
          addInitScript page "() => { window.__RELOADED = true; }"
          _ <- waitForFunction page "() => { if (!window.__RELOADED) window.location.reload(); return true; }"
          pure ()
    , testCase "should poll on interval" $
        withChromiumPage $ \page ->
          case milliseconds 30000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              handle <-
                waitForFunctionPolling
                  page
                  "() => { if (!window.__startTime) { window.__startTime = Date.now(); return false; } return Date.now() - window.__startTime; }"
                  timeout
                  100
              delta <- jsonValueNumber handle
              assertBool "at least polling interval" (delta >= 100)
    , testCase "should throw negative polling interval" $
        withChromiumPage $ \page ->
          case milliseconds 1000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertProtocolContains
                "Cannot poll with non-positive interval"
                (waitForFunctionPolling page "() => !!document.body" timeout (-10))
    , testCase "should avoid side effects after timeout" $
        withChromiumPage $ \page ->
          case milliseconds 1000 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              seen <- newIORef (0 :: Int)
              onConsole page $ \_ -> modifyIORef' seen (+ 1)
              assertProtocolContains
                "Timeout 1000ms exceeded"
                (waitForFunctionPolling page "() => { window.counter = (window.counter || 0) + 1; console.log(window.counter); }" timeout 1)
              saved <- readIORef seen
              case milliseconds 2000 of
                Nothing ->
                  assertFailure "timeout"
                Just pause ->
                  waitForTimeout page pause
              after <- readIORef seen
              assertEqual "no extra logs" saved after
    , testCase "should poll on raf" $
        withChromiumPage $ \page ->
          concurrently_
            (waitForFunction page "() => window.__FOO === 'hit'")
            (evaluateBool page "() => { window.__FOO = 'hit'; return true; }")
    , testCase "should return the success value as a JSHandle" $
        withChromiumPage $ \page -> do
          handle <- waitForFunction page "() => 5"
          assertEqual "5" (5 :: Double) =<< jsonValueNumber handle
    , testCase "should return the window as a success value" $
        withChromiumPage $ \page -> do
          _ <- waitForFunction page "() => window"
          pure ()
    , testCase "should fail with predicate throwing on first call" $
        withChromiumPage $ \page ->
          assertProtocolContains "oh my" (waitForFunction page "() => { throw new Error('oh my'); }")
    , testCase "should fail with predicate throwing sometimes" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Bad counter!"
            (waitForFunction page "() => { window.counter = (window.counter || 0) + 1; if (window.counter === 3) throw new Error('Bad counter!'); return window.counter === 5 ? 'result' : false; }")
    , testCase "should fail with ReferenceError on wrong page" $
        withChromiumPage $ \page ->
          assertProtocolContains "globalVar" (waitForFunction page "() => globalVar === 123")
    , testCase "should accept ElementHandle arguments" $
        withChromiumPage $ \page -> do
          setContent page "<div></div>"
          divEl <- requireHandle =<< querySelector page (selectorFromText "div")
          withAsync (waitForFunctionWithArg page "element => !element.parentElement" (elementArgument divEl)) $ \pending -> do
            _ <- evaluateOnElement divEl "el => el.remove()"
            _ <- wait pending
            pure ()
    , testCase "should respect timeout" $
        withChromiumPage $ \page ->
          case milliseconds 10 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              assertProtocolContains
                "Timeout 10ms exceeded"
                (waitForFunctionWith page "() => false" timeout)
    , testCase "should respect default timeout" $
        withChromiumPage $ \page ->
          case milliseconds 1 of
            Nothing ->
              assertFailure "timeout"
            Just timeout -> do
              setDefaultTimeout page timeout
              assertProtocolContains
                "Timeout 1ms exceeded"
                (waitForFunction page "() => false")
    , testCase "should disable timeout when its set to 0" $
        withChromiumPage $ \page ->
          case milliseconds 0 of
            Nothing ->
              assertFailure "timeout"
            Just timeout ->
              concurrently_
                (waitForFunctionPolling page "() => { window.__counter = (window.__counter || 0) + 1; return window.__injected; }" timeout 10)
                ( do
                    _ <- waitForFunction page "() => window.__counter > 10"
                    _ <- evaluateBool page "() => { window.__injected = true; return true; }"
                    pure ()
                )
    , testCase "should survive navigations" $
        withAssets $ \prefix page ->
          withAsync (waitForFunction page "() => window.__done") $ \pending -> do
            gotoEmpty prefix page
            gotoPage prefix page "consolelog.html"
            _ <- evaluateBool page "() => { window.__done = true; return true; }"
            _ <- wait pending
            pure ()
    , testCase "should work with multiline body" $
        withChromiumPage $ \page -> do
          handle <- waitForFunction page "() => true"
          assertEqual "true" True =<< jsonValueBool handle
    , testCase "should wait for predicate with arguments" $
        withChromiumPage $ \page -> do
          _ <-
            waitForFunctionWithArg
              page
              "({ arg1, arg2 }) => arg1 + arg2 === 3"
              (objectNumberArgument [("arg1", 1), ("arg2", 2)])
          pure ()
    , testCase "should not be called after finishing successfully" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          seen <- newIORef ([] :: [Text])
          onConsole page $ \message ->
            modifyIORef' seen (<> [consoleMessageText message])
          _ <- waitForFunction page "() => { console.log('waitForFunction1'); return true; }"
          _ <- reload page
          _ <- waitForFunction page "() => { console.log('waitForFunction2'); return true; }"
          _ <- reload page
          _ <- waitForFunction page "() => { console.log('waitForFunction3'); return true; }"
          logs <- readIORef seen
          assertEqual
            "logs"
            ["waitForFunction1", "waitForFunction2", "waitForFunction3"]
            (filter ("waitForFunction" `isInfixOf`) logs)
    , testCase "should throw when frame is detached" $
        withAssets $ \prefix page -> do
          frame <- attachFrame page prefix "frame1"
          withAsync (try (waitForFunctionOnFrame frame "() => false")) $ \pending -> do
            _ <- evaluate page "() => document.getElementById('frame1').remove()"
            result <- wait pending
            case result of
              Left (ProtocolRpc failure) ->
                assertBool
                  "detached"
                  ( "Frame was detached" `isInfixOf` protocolFailureMessage failure
                      || "Execution context was destroyed" `isInfixOf` protocolFailureMessage failure
                  )
              Left err ->
                assertFailure ("expected ProtocolRpc, got: " <> show err)
              Right _ ->
                assertFailure "expected waitForFunction to fail"
    ]

gotoEmpty :: Text -> Page -> Playwright ()
gotoEmpty prefix page = do
  _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
  pure ()

gotoPage :: Text -> Page -> Text -> Playwright ()
gotoPage prefix page name = do
  _ <- goto page (urlFromText (prefix <> "/" <> name)) defaultGotoOptions
  pure ()

attachFrame :: Page -> Text -> Text -> Playwright Frame
attachFrame page prefix frameId = do
  handle <-
    evaluateHandle
      page
      ( "async () => { const frame = document.createElement('iframe'); frame.src = '"
          <> prefix
          <> "/empty.html'; frame.id = '"
          <> frameId
          <> "'; document.body.appendChild(frame); await new Promise(x => frame.onload = x); return frame; }"
      )
  element <- requireHandle =<< asElement handle
  found <- contentFrameHandle element
  case found of
    Nothing ->
      assertFailure "expected frame"
    Just frame ->
      pure frame

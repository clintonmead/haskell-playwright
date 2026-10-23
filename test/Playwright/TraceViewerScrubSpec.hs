{-# LANGUAGE OverloadedStrings #-}

-- | Ports @tests/library/trace-viewer-scrub.spec.ts@.
--
-- Each case records a trace with the client, starts the driver's trace
-- viewer through @cli.js show-trace --port 0@, opens the printed URL in a
-- second browser context, and drives the viewer's playback controls.
module Playwright.TraceViewerScrubSpec
  ( tests
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)

import Data.Text (Text, pack)
import Data.Text qualified as T

import System.Environment (lookupEnv)
import System.IO (Handle, hGetLine)
import System.Process
  ( CreateProcess (std_out)
  , ProcessHandle
  , cleanupProcess
  , createProcess
  , proc
  , StdStream (CreatePipe)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Text.Read (readMaybe)
import UnliftIO.Exception (bracket)
import UnliftIO.Temporary (withSystemTempDirectory)

import Playwright
  ( AriaRole (AriaRole)
  , BoundingBox (boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  , Browser
  , CssPixel (cssPixelToScientific)
  , ExpectOptions (expectTimeout)
  , ExpectedText (ExpectedRegex, ExpectedString)
  , Exactness (Inexact)
  , GetByRoleOptions (getByRoleName, getByRoleSelected)
  , Key (Key)
  , Locator
  , Page
  , Playwright
  , Point (Point)
  , TracingStartOptions (TracingStartOptions)
  , click
  , cssPixelFromDouble
  , defaultExpectOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , expectHaveText
  , expectLocatorCount
  , expectLocatorWith
  , focus
  , getAttribute
  , getByRole
  , getByTestId
  , goto
  , keyboardPress
  , locatorBoundingBox
  , milliseconds
  , mouseClick
  , newContext
  , newPage
  , setContent
  , tracingStartWith
  , tracingStopTo
  )
import Playwright.Harness
  ( assertBool
  , assertFailure
  , locate
  , withChromiumBrowser
  )
import Playwright.Locator qualified as L
import Playwright.Url (urlFromText)

tests :: TestTree
tests =
  testGroup
    "TraceViewerScrub"
    [ testCase "should show playback controls" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickBody $ \viewer -> do
            expectVisible (getByRole viewer (AriaRole "slider") (namedRole "Playback position"))
            expectVisible (roleButton viewer "Play")
            expectVisible (roleButton viewer "Stop")
            expectVisible (roleButton viewer "Previous action")
            expectVisible (roleButton viewer "Next action")
            expectVisible (locate viewer ".playback-speed")
    , testCase "should navigate with next and previous buttons" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickThenInputBody $ \viewer -> do
            selectAction viewer "Set content" 0
            click (roleButton viewer "Next action")
            expectSelected viewer "Click"
            click (roleButton viewer "Next action")
            expectSelected viewer "Set content"
            click (roleButton viewer "Previous action")
            expectSelected viewer "Click"
    , testCase "should cycle playback speed" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickBody $ \viewer -> do
            let speed = locate viewer ".playback-speed"
            expectHaveText defaultExpectOptions speed (ExpectedString "1x")
            click speed
            expectHaveText defaultExpectOptions speed (ExpectedString "2x")
            click speed
            expectHaveText defaultExpectOptions speed (ExpectedString "0.5x")
            click speed
            expectHaveText defaultExpectOptions speed (ExpectedString "1x")
    , testCase "should stop and reset to first action" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickThenInputBody $ \viewer -> do
            selectAction viewer "Click" 0
            click (roleButton viewer "Stop")
            expectSelected viewer "Set content"
    , testCase "should support keyboard navigation on scrubber" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickThenInputBody $ \viewer -> do
            let scrubber = getByRole viewer (AriaRole "slider") (namedRole "Playback position")
            selectAction viewer "Set content" 0
            focus scrubber
            keyboardPress viewer (Key "ArrowRight")
            expectSelected viewer "Click"
            keyboardPress viewer (Key "ArrowRight")
            expectSelected viewer "Set content"
            keyboardPress viewer (Key "ArrowLeft")
            expectSelected viewer "Click"
    , testCase "should have tick marks on scrubber" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickThenInputBody $ \viewer ->
            expectLocatorCount defaultExpectOptions (locate viewer ".playback-tick") 3
    , testCase "should play and auto-stop at end" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickBody $ \viewer -> do
            selectAction viewer "Set content" 0
            click (roleButton viewer "Play")
            expectVisibleWithin 10000 (roleButton viewer "Play")
            expectSelected viewer "Click"
    , testCase "should update scrubber aria-valuenow" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickThenInputBody $ \viewer -> do
            let scrubber = getByRole viewer (AriaRole "slider") (namedRole "Playback position")
            selectAction viewer "Set content" 0
            val1 <- ariaValueNow scrubber
            selectAction viewer "Set content" 1
            val2 <- ariaValueNow scrubber
            assertBool
              ("expected aria-valuenow to increase, got " <> show (val1, val2))
              (val2 > val1)
    , testCase "should drag scrubber to select action" $
        withChromiumBrowser $ \browser ->
          withTraceViewer browser clickThenInputBody $ \viewer -> do
            let scrubber = getByRole viewer (AriaRole "slider") (namedRole "Playback position")
            box <- locatorBoundingBox scrubber
            case box of
              Nothing -> assertFailure "scrubber has no bounding box"
              Just b -> do
                let x = pixelValue (boundingBoxX b) + pixelValue (boundingBoxWidth b) * 0.95
                    y = pixelValue (boundingBoxY b) + pixelValue (boundingBoxHeight b) / 2
                mouseClick viewer (Point (cssPixelFromDouble x) (cssPixelFromDouble y))
            value <- ariaValueNow scrubber
            assertBool
              ("expected aria-valuenow above 50, got " <> show value)
              (value > 50)
    ]

-- Recording -----------------------------------------------------------------

clickBody :: Page -> Playwright ()
clickBody page = do
  setContent page "<button>Click me</button>"
  click (locate page "button")

clickThenInputBody :: Page -> Playwright ()
clickThenInputBody page = do
  clickBody page
  setContent page "<input/>"

-- | Records @body@ on a fresh context with snapshots and screenshots on,
-- then opens the resulting trace in the trace viewer.
withTraceViewer :: Browser -> (Page -> Playwright ()) -> (Page -> Playwright a) -> Playwright a
withTraceViewer browser body action = do
  actionCtx <- newContext browser
  actionPage <- newPage actionCtx
  withSystemTempDirectory "trace-viewer-scrub" $ \dir -> do
    let zipPath = dir <> "/trace.zip"
    tracingStartWith actionCtx (TracingStartOptions Nothing (Just True) (Just True) Nothing)
    body actionPage
    tracingStopTo actionCtx zipPath
    withShowTrace zipPath $ \traceUrl -> do
      viewerCtx <- newContext browser
      viewer <- newPage viewerCtx
      _ <- goto viewer (urlFromText traceUrl) defaultGotoOptions
      action viewer

-- Trace viewer process ------------------------------------------------------

withShowTrace :: FilePath -> (Text -> Playwright a) -> Playwright a
withShowTrace zipPath action =
  bracket
    (liftIO (spawnShowTrace zipPath))
    (\(handles, _) -> liftIO (cleanupProcess handles))
    (\(_, traceUrl) -> action traceUrl)

spawnShowTrace :: FilePath -> IO ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle), Text)
spawnShowTrace zipPath = do
  node <- envOrFail "PLAYWRIGHT_NODEJS_PATH"
  core <- envOrFail "PLAYWRIGHT_CORE_DIR"
  handles@(_, stdoutHandle, _, _) <-
    createProcess (proc node [core <> "/cli.js", "show-trace", "--port", "0", zipPath]) {std_out = CreatePipe}
  out <- maybe (fail "show-trace has no stdout") pure stdoutHandle
  traceUrl <- readListeningUrl out (20 :: Int)
  pure (handles, traceUrl)

readListeningUrl :: Handle -> Int -> IO Text
readListeningUrl h remaining
  | remaining <= 0 = fail "show-trace did not report a listening URL"
  | otherwise = do
      line <- hGetLine h
      case [w | w <- words line, "http://" `isPrefixOf` w] of
        (u : _) -> pure (pack u)
        [] -> readListeningUrl h (remaining - 1)

-- Viewer helpers ------------------------------------------------------------

namedRole :: Text -> GetByRoleOptions
namedRole name = defaultGetByRoleOptions {getByRoleName = Just name}

roleButton :: Page -> Text -> Locator
roleButton page name = getByRole page (AriaRole "button") (namedRole name)

actionsTree :: Page -> Locator
actionsTree page = getByTestId page "actions-tree"

selectAction :: Page -> Text -> Int -> Playwright ()
selectAction page actionTitle ordinal =
  click (L.nth (L.getByTitle (actionsTree page) actionTitle Inexact) ordinal)

selectedTreeItem :: Page -> Locator
selectedTreeItem page =
  L.getByRole (actionsTree page) (AriaRole "treeitem") defaultGetByRoleOptions {getByRoleSelected = Just True}

expectSelected :: Page -> Text -> Playwright ()
expectSelected page actionTitle =
  expectHaveText
    viewerExpectOptions
    (selectedTreeItem page)
    (ExpectedRegex (T.pack (regexEscape (T.unpack actionTitle))) "")

expectVisible :: Locator -> Playwright ()
expectVisible loc = expectLocatorWith viewerExpectOptions loc "to.be.visible"

expectVisibleWithin :: Double -> Locator -> Playwright ()
expectVisibleWithin ms loc =
  expectLocatorWith
    defaultExpectOptions {expectTimeout = milliseconds ms}
    loc
    "to.be.visible"

viewerExpectOptions :: ExpectOptions
viewerExpectOptions = defaultExpectOptions {expectTimeout = milliseconds 15000}

ariaValueNow :: Locator -> Playwright Double
ariaValueNow loc = do
  value <- getAttribute loc "aria-valuenow"
  case value >>= readMaybe . T.unpack of
    Just n -> pure n
    Nothing -> assertFailure ("aria-valuenow is not numeric: " <> show value)

pixelValue :: CssPixel -> Double
pixelValue = realToFrac . cssPixelToScientific

regexEscape :: String -> String
regexEscape = concatMap escape
  where
    escape c
      | c `elem` ("\\^$.|?*+()[]{}/" :: String) = ['\\', c]
      | otherwise = [c]

envOrFail :: String -> IO String
envOrFail name = do
  value <- lookupEnv name
  maybe (fail (name <> " is not set")) pure value

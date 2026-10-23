module Playwright.ScreencastSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Concurrent.MVar
  ( MVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , tryPutMVar
  )
import Control.Monad
  ( unless
  , void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( Page
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , ViewportHeight (ViewportHeight)
  , closeContext
  , closePage
  , defaultLaunchOptions
  , evaluate
  , newContextWith
  , newPage
  )
import Playwright.LaunchOptions
  ( LaunchOptions (launchOptionsArtifactsDir)
  )
import Playwright.NewContextOptions
  ( NewContextOptions (newContextRecordVideo, newContextViewport)
  , defaultNewContextOptions
  )
import Playwright.RecordVideo
  ( RecordVideo (RecordVideo)
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , withChromiumBrowser
  , withChromiumBrowserWith
  , withChromiumContextWith
  , withChromiumPage
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Screencast
  ( ScreencastFrame (screencastFrameData, screencastFrameViewportHeight, screencastFrameViewportWidth)
  , ScreencastOptions (screencastOptionsOnFrame, screencastOptionsPath, screencastOptionsSize)
  , ScreencastSize (ScreencastSize)
  , defaultScreencastOptions
  , screencastDispose
  , screencastStart
  , screencastStop
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  )
import Data.List
  ( isSuffixOf
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( finally
  , try
  )

-- Skipped upstream cases, not ported here:
--
-- * "empty video" and "start dispose stops recording": both decode the recorded
--   webm (frame size and pixel colour) with upstream's VideoPlayer. The Haskell
--   harness has no webm decoder, and the dev shell has no ffmpeg, so they stay
--   skipped, the same as the pixel cases in VideoSpec.

tests :: TestTree
tests =
  testGroup
    "screencast"
    [ testCase "screencast.start delivers frames via onFrame callback" $
        withChromiumContextWith viewport1000x400 $ \_ page -> do
          frames <- liftIO (newIORef [])
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsSize = Just (ScreencastSize 500 400)
                , screencastOptionsOnFrame = Just (recordFrames frames)
                }
          startPainting page
          waitForFrames frames 1
          frame <- firstFrame frames
          assertEqual "jpeg start" [0xFF, 0xD8] (ByteString.unpack (ByteString.take 2 (screencastFrameData frame)))
          assertEqual "jpeg size" (Just (500, 200)) (jpegDimensions (screencastFrameData frame))
    , testCase "applies backpressure while async onFrame callback is pending" $
        withChromiumPage $ \page -> do
          frames <- liftIO (newIORef [])
          gate <- liftIO (newEmptyMVar :: IO (MVar ()))
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsOnFrame = Just (\frame -> recordFrames frames frame >> liftIO (readMVar gate))
                }
          let
            checkBackpressure = do
              startPainting page
              waitForFrames frames 1
              liftIO (threadDelay 500000)
              blocked <- liftIO (frameCount frames)
              liftIO (threadDelay 500000)
              stillBlocked <- liftIO (frameCount frames)
              assertEqual "frames while gate is closed" blocked stillBlocked
              liftIO (putMVar gate ())
              waitForFrames frames (blocked + 1)
          -- Always release the gate, so a failed check cannot leave the handler blocked
          -- and stall the teardown.
          checkBackpressure `finally` liftIO (void (tryPutMVar gate ()))
    , testCase "onFrame receives viewport size" $
        withChromiumContextWith viewport1000x400 $ \_ page -> do
          frames <- liftIO (newIORef [])
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsOnFrame = Just (recordFrames frames)
                }
          startPainting page
          waitForFrames frames 1
          frame <- firstFrame frames
          assertEqual "viewport width" 1000 (screencastFrameViewportWidth frame)
          assertEqual "viewport height" 400 (screencastFrameViewportHeight frame)
    , testCase "start throws if screencast is already started" $
        withChromiumPage $ \page -> do
          _ <- screencastStart page defaultScreencastOptions
          result <- attempt (screencastStart page defaultScreencastOptions)
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "already started message"
                ("Screencast is already started" `isInfixOf` protocolFailureMessage failure)
            Left _ -> assertFailure "second start failed with an unexpected error"
            Right _ -> assertFailure "second start should have failed"
          screencastStop page
    , testCase "start allows restart with different options after stop" $
        withChromiumContextWith viewport1000x400 $ \_ page -> do
          firstFrames <- liftIO (newIORef [])
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsSize = Just (ScreencastSize 500 400)
                , screencastOptionsOnFrame = Just (recordFrames firstFrames)
                }
          startPainting page
          waitForFrames firstFrames 1
          screencastStop page
          secondFrames <- liftIO (newIORef [])
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsSize = Just (ScreencastSize 320 240)
                , screencastOptionsOnFrame = Just (recordFrames secondFrames)
                }
          startPainting page
          waitForFrames secondFrames 1
          frame <- firstFrame secondFrames
          assertEqual "restarted jpeg start" [0xFF, 0xD8] (ByteString.unpack (ByteString.take 2 (screencastFrameData frame)))
          screencastStop page
    , testCase "start returns a disposable that stops screencast" $
        withChromiumContextWith viewport1000x400 $ \_ page -> do
          frames <- liftIO (newIORef [])
          handle <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsOnFrame = Just (recordFrames frames)
                }
          startPainting page
          waitForFrames frames 1
          screencastDispose handle
          liftIO (threadDelay 200000)
          before <- liftIO (frameCount frames)
          liftIO (threadDelay 500000)
          after <- liftIO (frameCount frames)
          assertEqual "frames after dispose" before after
    , testCase "start should fail when another recording is in progress" $
        withChromiumPage $ \page -> do
          path <- recordingPath "in-progress"
          _ <- screencastStart page defaultScreencastOptions { screencastOptionsPath = Just path }
          result <- attempt (screencastStart page defaultScreencastOptions { screencastOptionsPath = Just path })
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "already started message"
                ("Screencast is already started" `isInfixOf` protocolFailureMessage failure)
            Left _ -> assertFailure "second start failed with an unexpected error"
            Right _ -> assertFailure "second start should have failed"
          screencastStop page
          liftIO (removeIfExists path)
    , testCase "stop should not fail when no recording is in progress" $
        withChromiumPage $ \page -> do
          screencastStop page
          assertBool "stop completed" True
    , testCase "start should finish when page is closed" $
        withChromiumPage $ \page -> do
          path <- recordingPath "page-closed"
          _ <- screencastStart page defaultScreencastOptions { screencastOptionsPath = Just path }
          closePage page
          result <- attempt (screencastStop page)
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "closed message"
                ("Target page, context or browser has been closed" `isInfixOf` protocolFailureMessage failure)
            Left _ -> assertFailure "stop failed with an unexpected error"
            Right _ -> assertBool "stop after close should fail" False
          liftIO (removeIfExists path)
    , testCase "start/stop twice without path creates two files in artifactsDir" $ do
        dir <- liftIO (freshDir "artifacts-dir")
        let artifactsDir = dir <> "/artifacts"
            size = ScreencastSize 800 800
        withChromiumBrowserWith defaultLaunchOptions {launchOptionsArtifactsDir = Just artifactsDir} $ \browser -> do
          context <- newContextWith browser defaultNewContextOptions {newContextViewport = Just (ViewportSize (ViewportWidth 800) (ViewportHeight 800))}
          page <- newPage context
          firstFrames <- liftIO (newIORef [])
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsPath = Just (dir <> "/video1.webm")
                , screencastOptionsSize = Just size
                , screencastOptionsOnFrame = Just (recordFrames firstFrames)
                }
          startPainting page
          waitForFrames firstFrames 1
          screencastStop page
          secondFrames <- liftIO (newIORef [])
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsPath = Just (dir <> "/video2.webm")
                , screencastOptionsSize = Just size
                , screencastOptionsOnFrame = Just (recordFrames secondFrames)
                }
          startPainting page
          waitForFrames secondFrames 1
          screencastStop page
          videos <- liftIO (webmFiles artifactsDir)
          assertEqual "webm files in artifactsDir" 2 (length videos)
          closeContext context
    , testCase "start should work when recordVideo is set" $ do
        autoDir <- liftIO (freshDir "record-video-auto")
        manualDir <- liftIO (freshDir "record-video-manual")
        withChromiumBrowser $ \browser -> do
          context <-
            newContextWith
              browser
              defaultNewContextOptions {newContextRecordVideo = Just (RecordVideo autoDir Nothing)}
          page <- newPage context
          frames <- liftIO (newIORef [])
          _ <-
            screencastStart
              page
              defaultScreencastOptions
                { screencastOptionsPath = Just (manualDir <> "/video.webm")
                , screencastOptionsOnFrame = Just (recordFrames frames)
                }
          startPainting page
          waitForFrames frames 1
          screencastStop page
          manualVideos <- liftIO (webmFiles manualDir)
          assertEqual "manual webm files" 1 (length manualVideos)
          closeContext context
          autoVideos <- liftIO (webmFiles autoDir)
          assertEqual "recordVideo webm files" 1 (length autoVideos)
    ]

viewport1000x400 :: NewContextOptions
viewport1000x400 =
  defaultNewContextOptions
    { newContextViewport = Just (ViewportSize (ViewportWidth 1000) (ViewportHeight 400))
    }

attempt :: Playwright a -> Playwright (Either PlaywrightError a)
attempt = try

-- | Paints the page on every animation frame so the screencast has frames to send.
startPainting :: Page -> Playwright ()
startPainting page =
  void $
    evaluate
      page
      "() => { let n = 0; const paint = () => { document.body.style.background = 'rgb(' + (n++ % 256) + ',0,0)'; requestAnimationFrame(paint); }; requestAnimationFrame(paint); }"

recordFrames :: IORef [ScreencastFrame] -> ScreencastFrame -> Playwright ()
recordFrames ref frame = liftIO (atomicModifyIORef' ref (\frames -> (frame : frames, ())))

frameCount :: IORef [ScreencastFrame] -> IO Int
frameCount ref = length <$> readIORef ref

firstFrame :: IORef [ScreencastFrame] -> Playwright ScreencastFrame
firstFrame ref = do
  frames <- liftIO (readIORef ref)
  case reverse frames of
    frame : _ -> pure frame
    [] -> assertFailure "no frame recorded"

waitForFrames :: IORef [ScreencastFrame] -> Int -> Playwright ()
waitForFrames ref wanted = liftIO (waitUntilIO ("frames >= " <> show wanted) ((>= wanted) <$> frameCount ref))

-- | Polls until the condition holds, failing after about ten seconds.
waitUntilIO :: String -> IO Bool -> IO ()
waitUntilIO label check = go (200 :: Int)
  where
    go :: Int -> IO ()
    go 0 = assertFailure ("timed out waiting for " <> label)
    go n = do
      ok <- check
      unless ok $ do
        threadDelay 50000
        go (n - 1)

-- | Reads the width and height from the SOF marker of a JPEG.
jpegDimensions :: ByteString -> Maybe (Int, Int)
jpegDimensions bytes = scan 2
  where
    byte :: Int -> Int
    byte i = fromIntegral (ByteString.index bytes i)
    be16 :: Int -> Int
    be16 i = byte i * 256 + byte (i + 1)
    scan :: Int -> Maybe (Int, Int)
    scan i
      | i + 9 > ByteString.length bytes = Nothing
      | byte i /= 0xFF = Nothing
      | byte (i + 1) `elem` [0xC0, 0xC1, 0xC2] = Just (be16 (i + 7), be16 (i + 5))
      | otherwise = scan (i + 2 + be16 (i + 2))

-- | A clean scratch directory under the system temp dir, named after the test.
freshDir :: String -> IO FilePath
freshDir name = do
  tmp <- getTemporaryDirectory
  let dir = tmp <> "/pw-screencast-" <> name
  removePathForcibly dir
  createDirectoryIfMissing True dir
  pure dir

webmFiles :: FilePath -> IO [FilePath]
webmFiles dir = do
  exists <- doesDirectoryExist dir
  names <- if exists then listDirectory dir else pure []
  pure (filter (".webm" `isSuffixOf`) names)

recordingPath :: String -> Playwright FilePath
recordingPath name = do
  dir <- liftIO getTemporaryDirectory
  pure (dir <> "/pw-screencast-" <> name <> ".webm")

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  if exists then removeFile path else pure ()

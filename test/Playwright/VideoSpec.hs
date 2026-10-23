module Playwright.VideoSpec
  ( tests
  ) where

import Control.Monad.IO.Class
  ( liftIO
  )
import Data.List
  ( isInfixOf
  )
import Data.Maybe
  ( isNothing
  )
import Data.Text qualified as Text
import UnliftIO.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  , removeDirectoryRecursive
  )
import UnliftIO.Exception
  ( bracket
  , try
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )
import Playwright.Browser
  ( closeBrowser
  , newContextWith
  )
import Playwright.BrowserContext
  ( closeContext
  , newPage
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
  , withChromiumContext
  , withChromiumContextWith
  )
import Playwright.NewContextOptions
  ( NewContextOptions (newContextRecordVideo, newContextViewport)
  , defaultNewContextOptions
  )
import Playwright.Page
  ( closePage
  )
import Playwright.RecordVideo
  ( RecordVideo (RecordVideo)
  )
import Playwright.Video
  ( pageVideo
  , videoDelete
  , videoPath
  , videoSaveAs
  )
import Playwright.ViewportSize
  ( ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize, viewportHeight, viewportWidth)
  , ViewportWidth (ViewportWidth)
  )
import Test.Tasty
  ( DependencyType (AllFinish)
  , TestTree
  , sequentialTestGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- Skipped: "should not throw without recordVideo.dir". The Haskell RecordVideo
-- type requires a directory, so the upstream "recordVideo: {}" shape cannot be
-- expressed. Omitting recordVideo entirely is covered by the first case.
--
-- Skipped: "should capture static page", "should pad a short frame to the video
-- size", "should continue recording main page after popup closes", the frame
-- capture and pixel cases, and the ffprobe size checks. They need an ffmpeg
-- frame reader that the port does not have yet.
--
-- Skipped: "should expose video path blank popup" and "should work for popups".
-- They need a popup event helper, which the harness does not have yet.
--
-- Skipped: "should throw if browser dies". It calls the test-only
-- browser._channel.killForTests, which has no binding here.

videoSize :: Int -> Int -> ViewportSize
videoSize width height =
  ViewportSize
    { viewportWidth = ViewportWidth width
    , viewportHeight = ViewportHeight height
    }

recordingOptions :: FilePath -> ViewportSize -> NewContextOptions
recordingOptions dir size =
  defaultNewContextOptions
    { newContextRecordVideo = Just (RecordVideo dir (Just size))
    , newContextViewport = Just size
    }

-- | Records one blank page, closes the context, and checks the video file exists.
expectVideoFile :: FilePath -> ViewportSize -> IO ()
expectVideoFile dir size =
  withChromiumContextWith (recordingOptions dir size) $ \context page -> do
    video <- pageVideo page
    case video of
      Nothing -> liftIO (assertFailure "expected a video for the page")
      Just v -> do
        path <- videoPath v
        assertBool "path is under recordVideo.dir" (dir `isInfixOf` path)
        closeContext context
        liftIO (doesFileExist path >>= assertBool "video file exists after close")

tests :: TestTree
tests =
  sequentialTestGroup
    "video"
    AllFinish
    [ testCase "should not have video by default" $
        withChromiumContext $ \_ page -> do
          video <- pageVideo page
          assertBool "no video" (isNothing video)
    , testCase "should expose video path" $
        withSystemTempDirectory "pw-video" $ \dir ->
          expectVideoFile dir (videoSize 320 240)
    , testCase "should expose video path blank page" $
        withSystemTempDirectory "pw-video" $ \dir ->
          expectVideoFile dir (videoSize 320 240)
    , testCase "should work with weird screen resolution" $
        withSystemTempDirectory "pw-video" $ \dir ->
          expectVideoFile dir (videoSize 1904 609)
    , testCase "should work with relative path for recordVideo.dir" $ do
        let relDir = "pw-video-relative-test" :: FilePath
        bracket
          (createDirectoryIfMissing True relDir)
          (\_ -> removeDirectoryRecursive relDir)
          (\_ -> expectVideoFile relDir (videoSize 320 240))
    , testCase "should delete video" $
        withSystemTempDirectory "pw-video" $ \dir ->
          withChromiumContextWith (recordingOptions dir (videoSize 320 240)) $ \context page -> do
            video <- pageVideo page
            case video of
              Nothing -> liftIO (assertFailure "expected a video for the page")
              Just v -> do
                closeContext context
                path <- videoPath v
                videoDelete v
                liftIO (doesFileExist path >>= assertBool "deleted" . not)
    , testCase "should saveAs video" $
        withSystemTempDirectory "pw-video" $ \dir ->
          withChromiumContextWith (recordingOptions dir (videoSize 320 240)) $ \context page -> do
            video <- pageVideo page
            case video of
              Nothing -> liftIO (assertFailure "expected a video for the page")
              Just v -> do
                closeContext context
                let saveAsPath = dir <> "/my-video.webm"
                videoSaveAs v saveAsPath
                liftIO (doesFileExist saveAsPath >>= assertBool "saved file exists")
    , testCase "should wait for video to finish if page was closed" $
        withSystemTempDirectory "pw-video" $ \dir ->
          withChromiumBrowser $ \browser -> do
            context <- newContextWith browser (recordingOptions dir (videoSize 320 240))
            page <- newPage context
            closePage page
            closeContext context
            closeBrowser browser
            files <- liftIO (listDirectory dir)
            -- Size check (320x240) is skipped until the ffprobe helper exists.
            assertEqual "one video file" 1 (length files)
    , testCase "should throw on browser close" $
        withSystemTempDirectory "pw-video" $ \dir ->
          withChromiumBrowser $ \browser -> do
            context <- newContextWith browser (recordingOptions dir (videoSize 320 240))
            page <- newPage context
            video <- pageVideo page
            closeBrowser browser
            case video of
              Nothing -> liftIO (assertFailure "expected a video for the page")
              Just v -> do
                result <- try (videoSaveAs v (dir <> "/saved-video-.webm"))
                case result of
                  Left (ProtocolRpc failure) ->
                    assertBool
                      "closed"
                      (Text.pack "has been closed" `Text.isInfixOf` protocolFailureMessage failure)
                  Left _ -> liftIO (assertFailure "unexpected error kind")
                  Right () -> liftIO (assertFailure "expected saveAs to fail")
    ]

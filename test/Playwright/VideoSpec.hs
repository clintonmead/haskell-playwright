module Playwright.VideoSpec
  ( tests
  ) where

import Control.Monad
  ( guard
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Bits
  ( countLeadingZeros
  , shiftL
  , shiftR
  , (.&.)
  )
import Data.ByteString qualified as BS
import Data.List
  ( isInfixOf
  )
import Data.Maybe
  ( isNothing
  , listToMaybe
  )
import Data.Text qualified as Text
import Data.Word
  ( Word8
  )
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
import Playwright.DeviceScaleFactor
  ( deviceScaleFactorFromInt
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
  ( NewContextOptions
      ( newContextDeviceScaleFactor
      , newContextHasTouch
      , newContextIsMobile
      , newContextNoDefaultViewport
      , newContextRecordVideo
      , newContextUserAgent
      , newContextViewport
      )
  , defaultNewContextOptions
  )
import Playwright.Page
  ( closePage
  )
import Playwright.RecordVideo
  ( RecordVideo (RecordVideo)
  )
import Playwright.UserAgent
  ( UserAgent (UserAgent)
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
-- size", "should continue recording main page after popup closes", "should
-- capture navigation", "should capture css transformation", "should scale
-- frames down to the requested size", "should capture static page in persistent
-- context", "should capture full viewport" (both variants), and "should work
-- with video+trace". They compare decoded frame pixels, and the port has no
-- WebM frame decoder yet. Only the container header is read (see
-- webmDimensions).
--
-- Skipped: "should expose video path blank popup", "should work for popups" and
-- "should not create video for internal pages". The first two need a popup
-- event helper, which the harness does not have yet. The last needs a
-- server route helper for Set-Cookie responses.
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

-- | A byte range inside a buffer: start offset and end offset (exclusive).
type Range = (Int, Int)

-- | Reads an EBML variable length integer used for element IDs (marker bits kept).
readId :: BS.ByteString -> Int -> Maybe (Int, Int)
readId bytes pos = do
  first <- byteAt bytes pos
  n <- vintLength first
  guard (pos + n <= BS.length bytes)
  pure (bigEndian bytes pos n, n)

-- | Reads an EBML size. Nothing means the unknown-size marker.
readSize :: BS.ByteString -> Int -> Maybe (Maybe Int, Int)
readSize bytes pos = do
  first <- byteAt bytes pos
  n <- vintLength first
  guard (pos + n <= BS.length bytes)
  let marker = fromIntegral (0xFF `shiftR` n :: Word8) :: Int
      value = (fromIntegral first .&. marker) * (256 ^ (n - 1)) + bigEndian bytes (pos + 1) (n - 1)
      unknown = value == (1 `shiftL` (7 * n)) - 1
  pure (if unknown then Nothing else Just value, n)

vintLength :: Word8 -> Maybe Int
vintLength byte
  | byte == 0 = Nothing
  | otherwise = Just (countLeadingZeros byte + 1)

byteAt :: BS.ByteString -> Int -> Maybe Word8
byteAt bytes pos
  | pos < 0 || pos >= BS.length bytes = Nothing
  | otherwise = Just (BS.index bytes pos)

bigEndian :: BS.ByteString -> Int -> Int -> Int
bigEndian bytes start count =
  foldl' (\acc i -> acc * 256 + fromIntegral (BS.index bytes i)) 0 [start .. start + count - 1]

-- | Lists the child elements of a master element as (id, payload range).
children :: BS.ByteString -> Range -> [(Int, Range)]
children bytes (start, end) = go start
  where
    go :: Int -> [(Int, Range)]
    go pos
      | pos >= end = []
      | otherwise = case readId bytes pos of
          Nothing -> []
          Just (elementId, idLen) -> case readSize bytes (pos + idLen) of
            Nothing -> []
            Just (size, sizeLen) ->
              let payloadStart = pos + idLen + sizeLen
                  payloadEnd = maybe end (\s -> min end (payloadStart + s)) size
               in (elementId, (payloadStart, payloadEnd)) : go payloadEnd

childRange :: BS.ByteString -> Int -> Range -> Maybe Range
childRange bytes elementId parent =
  listToMaybe [range | (i, range) <- children bytes parent, i == elementId]

childUnsigned :: BS.ByteString -> Int -> Range -> Maybe Int
childUnsigned bytes elementId parent = do
  (s, e) <- childRange bytes elementId parent
  pure (bigEndian bytes s (e - s))

-- | Reads PixelWidth and PixelHeight from the first video track of a WebM file,
-- by walking Segment, Tracks, TrackEntry and Video. ffprobe is not available to
-- the test suite, so this stands in for it when checking recorded dimensions.
webmDimensions :: FilePath -> IO (Maybe (Int, Int))
webmDimensions path = do
  bytes <- BS.readFile path
  let whole :: Range
      whole = (0, BS.length bytes)
      dims :: Maybe (Int, Int)
      dims = do
        segment <- childRange bytes 0x18538067 whole
        tracks <- childRange bytes 0x1654AE6B segment
        let videoEntries = [v | (0xAE, entry) <- children bytes tracks, Just v <- [childRange bytes 0xE0 entry]]
        video <- listToMaybe videoEntries
        width <- childUnsigned bytes 0xB0 video
        height <- childUnsigned bytes 0xBA video
        pure (width, height)
  pure dims

-- | Records one blank page with the options built from the video directory, and
-- checks the recorded WebM header dimensions.
expectRecordedSize :: (FilePath -> NewContextOptions) -> (Int, Int) -> IO ()
expectRecordedSize mkOptions (w, h) =
  withSystemTempDirectory "pw-video" $ \dir ->
    withChromiumBrowser $ \browser -> do
      context <- newContextWith browser (mkOptions dir)
      page <- newPage context
      video <- pageVideo page
      case video of
        Nothing -> liftIO (assertFailure "expected a video for the page")
        Just v -> do
          closeContext context
          path <- videoPath v
          dims <- liftIO (webmDimensions path)
          liftIO (assertEqual "video dimensions" (Just (w, h)) dims)

iPhone6Options :: FilePath -> NewContextOptions
iPhone6Options dir =
  defaultNewContextOptions
    { newContextUserAgent =
        Just (UserAgent "Mozilla/5.0 (iPhone; CPU iPhone OS 11_0 like Mac OS X) AppleWebKit/604.1.38 (KHTML, like Gecko) Version/26.5 Mobile/15A372 Safari/604.1")
    , newContextViewport = Just (videoSize 375 667)
    , newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 2)
    , newContextIsMobile = Just True
    , newContextHasTouch = Just True
    , newContextRecordVideo = Just (RecordVideo dir Nothing)
    }

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
            assertEqual "one video file" 1 (length files)
            case files of
              [file] -> do
                dims <- liftIO (webmDimensions (dir <> "/" <> file))
                liftIO (assertEqual "video dimensions" (Just (320, 240)) dims)
              _ -> liftIO (assertFailure "expected exactly one file")
    , testCase "should close ffmpeg even if there were no frames" $
        withSystemTempDirectory "pw-video" $ \dir ->
          withChromiumBrowser $ \browser -> do
            context <- newContextWith browser (recordingOptions dir (videoSize 320 240))
            page1 <- newPage context
            closePage page1
            page2 <- newPage context
            closePage page2
            closeContext context
            closeBrowser browser
            files <- liftIO (listDirectory dir)
            assertEqual "two video files" 2 (length files)
    , testCase "should use viewport scaled down to fit into 800x800 as default size" $
        expectRecordedSize
          (\dir ->
            defaultNewContextOptions
              { newContextRecordVideo = Just (RecordVideo dir Nothing)
              , newContextViewport = Just (videoSize 1600 1200)
              })
          (800, 600)
    , testCase "should be 800x450 by default" $
        expectRecordedSize
          (\dir -> defaultNewContextOptions {newContextRecordVideo = Just (RecordVideo dir Nothing)})
          (800, 450)
    , testCase "should be 800x600 with null viewport" $
        expectRecordedSize
          (\dir ->
            defaultNewContextOptions
              { newContextRecordVideo = Just (RecordVideo dir Nothing)
              , newContextNoDefaultViewport = Just True
              })
          (800, 600)
    , testCase "should emulate an iphone" $
        expectRecordedSize iPhone6Options (374, 666)
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

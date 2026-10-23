module Playwright.PageScreenshotSpec
  ( tests
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( BoundingBox (BoundingBox)
  , ScreenshotOptions
      ( screenshotOptionsClip
      , screenshotOptionsFullPage
      , screenshotOptionsOmitBackground
      , screenshotOptionsPath
      , screenshotOptionsQuality
      , screenshotOptionsType
      )
  , ScreenshotType (ScreenshotJpeg, ScreenshotPng, ScreenshotWebp)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , cssPixelFromInt
  , defaultGotoOptions
  , defaultScreenshotOptions
  , evaluate
  , goto
  , screenshot
  , setViewportSize
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( withAssets
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
import UnliftIO.Async
  ( mapConcurrently
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.Temporary
  ( withSystemTempDirectory
  )

tests :: TestTree
tests =
  testGroup
    "page-screenshot"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
          assertBool "non-empty" (BS.length image > 100)
    , testCase "should produce a valid webp screenshot" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page (withType ScreenshotWebp)
          assertBool "webp signature" (isWebp image)
    , testCase "should render white background on jpeg file" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <-
            screenshot
              page
              defaultScreenshotOptions
                { screenshotOptionsType = Just ScreenshotJpeg
                , screenshotOptionsOmitBackground = Just True
                }
          assertBool "jpeg signature" (jpegMagic `BS.isPrefixOf` image)
    , testCase "quality option should throw for png" $
        withAssets $ \_ page -> do
          result <-
            try
              (screenshot page defaultScreenshotOptions {screenshotOptionsQuality = Just 10})
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "png quality unsupported"
                ("options.quality is unsupported for the png" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected png quality to throw"
    , testCase "zero quality option should throw for png" $
        withAssets $ \_ page -> do
          result <-
            try
              (screenshot page defaultScreenshotOptions {screenshotOptionsQuality = Just 0, screenshotOptionsType = Just ScreenshotPng})
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "zero png quality unsupported"
                ("options.quality is unsupported for the png" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected zero png quality to throw"
    , testCase "quality option should work for jpeg" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          zeroQuality <- screenshot page (withQuality ScreenshotJpeg 0)
          highQuality <- screenshot page (withQuality ScreenshotJpeg 100)
          assertBool "low quality is smaller" (BS.length zeroQuality < BS.length highQuality)
    , testCase "quality option should work for webp" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          lowQuality <- screenshot page (withQuality ScreenshotWebp 0)
          highQuality <- screenshot page (withQuality ScreenshotWebp 100)
          assertBool "low quality is smaller" (BS.length lowQuality < BS.length highQuality)
    , testCase "quality option should throw for webp when out of range" $
        withAssets $ \_ page -> do
          result <-
            try
              (screenshot page (withQuality ScreenshotWebp 101))
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "webp quality out of range"
                ("Expected options.quality to be between 0 and 100" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected webp quality 101 to throw"
    , testCase "should clip rect" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page (withClip 50 100 150 100)
          assertEqual "clipped png size" (150, 100) (pngSize image)
    , testCase "should clip rect with fullPage" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- evaluate page "() => window.scrollBy(150, 200)"
          image <- screenshot page ((withClip 50 100 150 100) {screenshotOptionsFullPage = Just True})
          assertEqual "clipped fullPage png size" (150, 100) (pngSize image)
    , testCase "should clip elements to the viewport" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page (withClip 50 450 1000 100)
          assertEqual "clipped to the viewport" (450, 50) (pngSize image)
    , testCase "should throw on clip outside the viewport" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          result <- try (screenshot page (withClip 50 650 100 100))
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "clip outside the viewport"
                ("Clipped area is either empty or outside the resulting image" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected clip outside the viewport to throw"
    , testCase "should throw on a negative clip size" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          result <- try (screenshot page (withClip 50 50 (-100) 100))
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "negative clip width"
                ("Expected options.clip.width to be greater than 0" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected negative clip width to throw"
    , testCase "should run in parallel" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          images <- mapConcurrently (\i -> screenshot page (withClip (50 * i) 0 50 50)) [0, 1, 2]
          mapM_ (assertEqual "grid cell size" (50, 50) . pngSize) images
    , testCase "should work with odd clip size" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page (withClip 0 0 11 11)
          assertEqual "odd clip png size" (11, 11) (pngSize image)
    , testCase "path option should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          withSystemTempDirectory "pw-screenshot-path" $ \dir -> do
            let path = dir <> "/shot.png"
            image <- screenshot page defaultScreenshotOptions {screenshotOptionsPath = Just path}
            saved <- liftIO (BS.readFile path)
            assertBool "png signature" (pngMagic `BS.isPrefixOf` saved)
            assertEqual "saved bytes match returned bytes" image saved
    , testCase "path option should create subdirectories" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          withSystemTempDirectory "pw-screenshot-path" $ \dir -> do
            let path = dir <> "/nested/deeper/shot.png"
            image <- screenshot page defaultScreenshotOptions {screenshotOptionsPath = Just path}
            saved <- liftIO (BS.readFile path)
            assertEqual "saved bytes match returned bytes" image saved
    , testCase "path option should detect jpeg" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          withSystemTempDirectory "pw-screenshot-path" $ \dir -> do
            let path = dir <> "/shot.jpeg"
            image <- screenshot page defaultScreenshotOptions {screenshotOptionsPath = Just path}
            saved <- liftIO (BS.readFile path)
            assertBool "jpeg signature" (jpegMagic `BS.isPrefixOf` saved)
            assertEqual "saved bytes match returned bytes" image saved
    , testCase "path option should detect webp" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          withSystemTempDirectory "pw-screenshot-path" $ \dir -> do
            let path = dir <> "/shot.webp"
            image <- screenshot page defaultScreenshotOptions {screenshotOptionsPath = Just path}
            saved <- liftIO (BS.readFile path)
            assertBool "webp signature" (isWebp saved)
            assertEqual "saved bytes match returned bytes" image saved
    , testCase "path option should throw for unsupported mime type" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          withSystemTempDirectory "pw-screenshot-path" $ \dir -> do
            result <-
              try
                (screenshot page defaultScreenshotOptions {screenshotOptionsPath = Just (dir <> "/shot.txt")})
            case result of
              Left (DecodeFailure message) ->
                assertEqual "unsupported mime type" "path: unsupported mime type \"text/plain\"" message
              Left err ->
                assertFailure ("expected DecodeFailure, got: " <> show err)
              Right _ ->
                assertFailure "expected unsupported mime type to throw"
    , testCase "should prefer type over extension" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          withSystemTempDirectory "pw-screenshot-path" $ \dir -> do
            let path = dir <> "/shot.png"
            image <-
              screenshot
                page
                defaultScreenshotOptions
                  { screenshotOptionsType = Just ScreenshotJpeg
                  , screenshotOptionsPath = Just path
                  }
            saved <- liftIO (BS.readFile path)
            assertBool "jpeg signature" (jpegMagic `BS.isPrefixOf` image)
            assertEqual "saved bytes match returned bytes" image saved
    ]

withClip :: Int -> Int -> Int -> Int -> ScreenshotOptions
withClip x y width height =
  defaultScreenshotOptions
    { screenshotOptionsClip =
        Just
          ( BoundingBox
              (cssPixelFromInt x)
              (cssPixelFromInt y)
              (cssPixelFromInt width)
              (cssPixelFromInt height)
          )
    }

pngSize :: ByteString -> (Int, Int)
pngSize image =
  (bigEndian32 (BS.take 4 (BS.drop 16 image)), bigEndian32 (BS.take 4 (BS.drop 20 image)))

bigEndian32 :: ByteString -> Int
bigEndian32 =
  BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

withType :: ScreenshotType -> ScreenshotOptions
withType screenshotType =
  defaultScreenshotOptions {screenshotOptionsType = Just screenshotType}

withQuality :: ScreenshotType -> Int -> ScreenshotOptions
withQuality screenshotType quality =
  defaultScreenshotOptions
    { screenshotOptionsType = Just screenshotType
    , screenshotOptionsQuality = Just quality
    }

isWebp :: ByteString -> Bool
isWebp image =
  "RIFF" `BS.isPrefixOf` image && "WEBP" `BS.isPrefixOf` BS.drop 8 image

pngMagic :: ByteString
pngMagic =
  "\137PNG\r\n\26\n"

jpegMagic :: ByteString
jpegMagic =
  "\255\216\255"

module Playwright.PageScreenshotSpec
  ( tests
  ) where

import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Data.Text
  ( isInfixOf
  )
import Playwright
  ( ScreenshotOptions
      ( screenshotOptionsOmitBackground
      , screenshotOptionsQuality
      , screenshotOptionsType
      )
  , ScreenshotType (ScreenshotJpeg, ScreenshotPng, ScreenshotWebp)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , defaultGotoOptions
  , defaultScreenshotOptions
  , goto
  , screenshot
  , setViewportSize
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( withAssets
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
import UnliftIO.Exception
  ( try
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
    ]

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

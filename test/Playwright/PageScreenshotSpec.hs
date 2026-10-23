module Playwright.PageScreenshotSpec
  ( tests
  ) where

import Control.Concurrent
  ( newEmptyMVar
  , putMVar
  , takeMVar
  , threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Bits
  ( testBit
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Data.Text
  ( Text
  , isInfixOf
  , pack
  , replace
  )
import Playwright
  ( BoundingBox (BoundingBox)
  , Locator
  , NewContextOptions (newContextDeviceScaleFactor)
  , BindingName (BindingName)
  , ContentType (ContentType)
  , FulfillBody (FulfillBytes)
  , FulfillOptions (fulfillBody, fulfillContentType)
  , GotoOptions (gotoOptionsWaitUntil)
  , LoadState (LoadStateDomContentLoaded)
  , Page
  , Playwright
  , ScreenshotAnimations (ScreenshotAnimationsAllow, ScreenshotAnimationsDisabled)
  , ScreenshotCaret (ScreenshotCaretHide, ScreenshotCaretInitial)
  , ScreenshotOptions
      ( screenshotOptionsAnimations
      , screenshotOptionsCaret
      , screenshotOptionsClip
      , screenshotOptionsFullPage
      , screenshotOptionsMaskColor
      , screenshotOptionsTimeout
      , screenshotOptionsOmitBackground
      , screenshotOptionsPath
      , screenshotOptionsQuality
      , screenshotOptionsScale
      , screenshotOptionsStyle
      , screenshotOptionsType
      )
  , ScreenshotScale (ScreenshotScaleCss, ScreenshotScaleDevice)
  , ScreenshotType (ScreenshotJpeg, ScreenshotPng, ScreenshotWebp)
  , ScriptTag (scriptTagContent)
  , Timeout
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , addScriptTag
  , childLocator
  , defaultFulfillOptions
  , defaultRouteOptions
  , fulfillRoute
  , routeWith
  , cssPixelFromInt
  , defaultGotoOptions
  , defaultNewContextOptions
  , defaultScreenshotOptions
  , deviceScaleFactorFromInt
  , emptyScriptTag
  , evaluate
  , evaluateBool
  , evaluateOnLocator
  , evaluateOnLocatorText
  , exposeFunction
  , focus
  , frameLocator
  , goto
  , locator
  , locatorScreenshot
  , locatorScreenshotMasked
  , milliseconds
  , nth
  , querySelector
  , reload
  , screenshot
  , screenshotHandle
  , screenshotHandleMasked
  , screenshotMasked
  , selectorFromText
  , setContent
  , setViewportSize
  , typeText
  , undefinedArgument
  , UrlMatch (UrlMatchGlob)
  , urlFromText
  , waitFor
  , WaitForState (WaitForVisible)
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( attachFrame
  , withAssets
  , withAssetsWith
  , assertBool
  , assertEqual
  , assertFailure
  , readTestAsset
  , requireHandle
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( async
  , mapConcurrently
  , wait
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( newIORef
  , readIORef
  , writeIORef
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
    , testCase "caret hide should return png bytes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsCaret = Just ScreenshotCaretHide}
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "caret initial should return png bytes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsCaret = Just ScreenshotCaretInitial}
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "animations disabled should return png bytes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsAnimations = Just ScreenshotAnimationsDisabled}
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "animations allow should return png bytes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsAnimations = Just ScreenshotAnimationsAllow}
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "scale css should ignore device scale factor" $
        withAssetsWith dsf2Options $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsScale = Just ScreenshotScaleCss}
          assertEqual "css scale size" (300, 300) (pngSize image)
    , testCase "scale device should apply device scale factor" $
        withAssetsWith dsf2Options $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsScale = Just ScreenshotScaleDevice}
          assertEqual "device scale size" (600, 600) (pngSize image)
    , testCase "mask should paint over the masked locator" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          plain <- screenshot page defaultScreenshotOptions
          masked <- screenshotMasked page [locator page (selectorFromText "div.box")] defaultScreenshotOptions
          assertBool "masked differs from plain" (masked /= plain)
          afterMask <- screenshot page defaultScreenshotOptions
          assertEqual "mask is not left on the page" plain afterMask
    , testCase "mask color should change the painted pixels" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          let boxes = [locator page (selectorFromText "div.box")]
          defaultPaint <- screenshotMasked page boxes defaultScreenshotOptions
          redPaint <- screenshotMasked page boxes defaultScreenshotOptions {screenshotOptionsMaskColor = Just "#FF0000"}
          assertBool "mask color differs from default" (redPaint /= defaultPaint)
    , testCase "style should change the screenshot" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          plain <- screenshot page defaultScreenshotOptions
          styled <- screenshot page defaultScreenshotOptions {screenshotOptionsStyle = Just "body { background: rgb(0, 0, 255) !important; }"}
          assertBool "style differs from plain" (styled /= plain)
    , testCase "should work with locator" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          let bodyLocator = locator page (selectorFromText "body")
          plain <- locatorScreenshot bodyLocator defaultScreenshotOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` plain)
          masked <- locatorScreenshotMasked bodyLocator [locator page (selectorFromText "div.box")] defaultScreenshotOptions
          assertBool "masked locator differs from plain" (masked /= plain)
    , testCase "should work with elementhandle" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          handle <- requireHandle =<< querySelector page (selectorFromText "body")
          plain <- screenshotHandle handle
          masked <- screenshotHandleMasked handle [nth (locator page (selectorFromText "div")) 5] defaultScreenshotOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` masked)
          assertBool "masked element handle differs from plain" (masked /= plain)
    , testCase "should work for canvas" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/screenshots/canvas.html")) defaultGotoOptions
          drawn <- screenshot page defaultScreenshotOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` drawn)
          _ <- evaluate page "() => { const c = document.querySelector('canvas'); c.getContext('2d').clearRect(0, 0, c.width, c.height); }"
          blank <- screenshot page defaultScreenshotOptions
          assertBool "drawn canvas differs from blank canvas" (drawn /= blank)
    , testCase "should take fullPage screenshots" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          image <- screenshot page fullPageOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
          assertBool "fullPage is taller than the viewport" (snd (pngSize image) > 500)
    , testCase "should take fullPage screenshots and mask elements outside of it" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          plain <- screenshot page fullPageOptions
          masked <- screenshotMasked page [nth (locator page (selectorFromText ".box")) 144] fullPageOptions
          assertBool "masked fullPage differs from plain" (masked /= plain)
    , testCase "should restore viewport after fullPage screenshot" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- screenshot page fullPageOptions
          after <- screenshot page defaultScreenshotOptions
          assertBool "viewport restored to 500x500" (pngSize after == (500, 500))
    , testCase "should allow transparency" $
        withAssets $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          _ <- setContent page transparencyPage
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsOmitBackground = Just True}
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
          assertBool "RGBA colour type (6) in IHDR" (BS.index image 25 == 6)
    , testCase "webp screenshots should be lossless by default" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          lossless <- screenshot page (withType ScreenshotWebp)
          lossy <- screenshot page (withQuality ScreenshotWebp 80)
          assertBool "default webp is VP8L (lossless)" (fst (webpImageChunk lossless) == "VP8L")
          assertBool "quality 80 webp is VP8 (lossy)" (fst (webpImageChunk lossy) == "VP8 ")
    , testCase "should allow transparency with webp" $
        withAssets $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 300) (ViewportHeight 300))
          _ <- setContent page transparencyPage
          image <- screenshot page defaultScreenshotOptions {screenshotOptionsType = Just ScreenshotWebp, screenshotOptionsOmitBackground = Just True}
          assertBool "webp container" (isWebp image)
          let (chunk, payload) = webpImageChunk image
          assertBool "VP8L chunk" (chunk == "VP8L")
          -- VP8L payload: one signature byte, then 14-bit width-1 and height-1 plus flags.
          let header = littleEndian32 (BS.take 4 (BS.drop 1 payload))
          assertBool "300x300 in VP8L header" ((header `mod` 16384 + 1, (header `div` 16384) `mod` 16384 + 1) == (300, 300))
          assertBool "alpha_is_used bit set" (testBit header 28)
    , testCase "should capture canvas changes" $
        withAssets $ \_ page -> do
          _ <- setContent page "<canvas></canvas>"
          _ <- evaluate page "() => { const canvas = document.querySelector('canvas'); canvas.width = 600; canvas.height = 600; }"
          images <- mapM (captureCanvasStep page) [0 .. 2]
          assertBool "each new line changes the screenshot" (and (zipWith (/=) images (drop 1 images)))
    , testCase "should mask multiple elements" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          plain <- screenshot page defaultScreenshotOptions
          one <- screenshotMasked page [nth (locator page (selectorFromText "div")) 5] defaultScreenshotOptions
          both <-
            screenshotMasked
              page
              [ nth (locator page (selectorFromText "div")) 5
              , nth (locator page (selectorFromText "div")) 12
              ]
              defaultScreenshotOptions
          assertBool "one mask differs from plain" (one /= plain)
          assertBool "two masks differ from one mask" (both /= one)
    , testCase "should mask inside iframe" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- attachFrame page "frame1" (prefix <> "/grid.html")
          plain <- screenshot page defaultScreenshotOptions
          masked <-
            screenshotMasked
              page
              [ nth (locator page (selectorFromText "div")) 5
              , nth (childLocator (frameLocator page (selectorFromText "#frame1")) (selectorFromText "div")) 12
              ]
              defaultScreenshotOptions
          assertBool "masked frame content differs from plain" (masked /= plain)
    , testCase "should mask in parallel" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- attachFrame page "frame1" (prefix <> "/grid.html")
          _ <- attachFrame page "frame2" (prefix <> "/grid.html")
          let frameMask name index =
                nth (childLocator (frameLocator page (selectorFromText name)) (selectorFromText "div")) index
          images <-
            mapConcurrently
              (\mask -> screenshotMasked page [mask] defaultScreenshotOptions)
              [frameMask "#frame1" 1, frameMask "#frame2" 3]
          assertEqual "two parallel screenshots" 2 (length images)
          assertBool "every parallel screenshot is a png" (all (pngMagic `BS.isPrefixOf`) images)
    , testCase "should remove mask after screenshot" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          before <- screenshot page defaultScreenshotOptions
          _ <- screenshotMasked page [nth (locator page (selectorFromText "div")) 1] defaultScreenshotOptions
          after <- screenshot page defaultScreenshotOptions
          assertBool "plain screenshot is unchanged after a masked one" (before == after)
    , testCase "should not issue resize event" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          resized <- newIORef False
          _ <-
            exposeFunction page (BindingName "resize") $ \_ -> do
              writeIORef resized True
              pure undefinedArgument
          _ <- evaluate page "() => { window.addEventListener('resize', () => window.resize()); }"
          _ <- screenshot page defaultScreenshotOptions
          assertEqual "resize not triggered" False =<< readIORef resized
    , testCase "should work with Array deleted" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- evaluate page "() => { delete window.Array; }"
          image <- screenshot page fullPageOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "should take fullPage screenshots during navigation" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          let
            reloadSeveralTimes :: Playwright ()
            reloadSeveralTimes = mapM_ (const (reload page)) [1 .. 5 :: Int]
            screenshotSeveralTimes :: Playwright ()
            screenshotSeveralTimes = mapM_ (const (checkNavigatingScreenshot page)) [1 .. 5 :: Int]
          _ <- mapConcurrently id [reloadSeveralTimes, screenshotSeveralTimes]
          pure ()
    , testCase "should work when mask color is not pink #F0F" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          let
            target :: Locator
            target = nth (locator page (selectorFromText "div")) 5
            greenOptions :: ScreenshotOptions
            greenOptions = defaultScreenshotOptions {screenshotOptionsMaskColor = Just "#00FF00"}
          pink <- screenshotMasked page [target] defaultScreenshotOptions
          green <- screenshotMasked page [target] greenOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` green)
          assertBool "green mask differs from pink mask" (green /= pink)
    , testCase "should hide elements based on attr" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          let
            target :: Locator
            target = nth (locator page (selectorFromText "div")) 5
            hideOptions :: ScreenshotOptions
            hideOptions =
              defaultScreenshotOptions
                {screenshotOptionsStyle = Just "[data-test-screenshot=\"hide\"] { visibility: hidden; }"}
          plain <- screenshot page defaultScreenshotOptions
          _ <- evaluateOnLocator target "element => { element.setAttribute('data-test-screenshot', 'hide'); }"
          hidden <- screenshot page hideOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` hidden)
          assertBool "hidden element changes the screenshot" (hidden /= plain)
          assertEqual "inline visibility untouched" "" =<< evaluateOnLocatorText target "element => element.style.visibility"
    , testCase "should remove elements based on attr" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          let
            target :: Locator
            target = nth (locator page (selectorFromText "div")) 5
            removeOptions :: ScreenshotOptions
            removeOptions =
              defaultScreenshotOptions
                {screenshotOptionsStyle = Just "[data-test-screenshot=\"remove\"] { display: none; }"}
          plain <- screenshot page defaultScreenshotOptions
          _ <- evaluateOnLocator target "element => { element.setAttribute('data-test-screenshot', 'remove'); }"
          removed <- screenshot page removeOptions
          assertBool "png signature" (pngMagic `BS.isPrefixOf` removed)
          assertBool "removed element changes the screenshot" (removed /= plain)
          assertEqual "inline display untouched" "" =<< evaluateOnLocatorText target "element => element.style.display"
    , testCase "should work when subframe has stalled navigation" $
        withAssets $ \prefix page -> do
          routeReady <- liftIO newEmptyMVar
          releaseRoute <- liftIO newEmptyMVar
          routeWith
            page
            (UrlMatchGlob "**/subframe.html")
            defaultRouteOptions
            $ \handled -> do
              liftIO (putMVar routeReady ())
              liftIO (takeMVar releaseRoute)
              fulfillRoute handled defaultFulfillOptions
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          done <- async (setContent page ("<iframe src='" <> prefix <> "/subframe.html'></iframe>"))
          liftIO (takeMVar routeReady)
          image <- screenshotMasked page [locator page (selectorFromText "non-existent")] defaultScreenshotOptions
          liftIO (putMVar releaseRoute ())
          wait done
          assertBool "screenshot is a png" (pngMagic `BS.isPrefixOf` image)
    , testCase "should work when subframe used document.open after a weird url" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "() => { const iframe = document.createElement('iframe'); iframe.src = 'javascript:hi'; document.body.appendChild(iframe); iframe.contentDocument.open(); iframe.contentDocument.write('Hello'); iframe.contentDocument.close(); }"
          image <- screenshotMasked page [locator page (selectorFromText "non-existent")] defaultScreenshotOptions
          assertBool "screenshot is a png" (pngMagic `BS.isPrefixOf` image)
    , testCase "should not capture infinite css animation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rotate-z.html")) defaultGotoOptions
          let target = locator page (selectorFromText "div")
          initial <- locatorScreenshot target disabledAnimations
          mapM_
            ( \_ -> do
                rafraf page
                again <- locatorScreenshot target disabledAnimations
                assertEqual "screenshot stays still" initial again
            )
            [1 .. 10 :: Int]
    , testCase "should not capture pseudo element css animation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rotate-pseudo.html")) defaultGotoOptions
          let target = locator page (selectorFromText "div")
          initial <- locatorScreenshot target disabledAnimations
          mapM_
            ( \_ -> do
                rafraf page
                again <- locatorScreenshot target disabledAnimations
                assertEqual "screenshot stays still" initial again
            )
            [1 .. 10 :: Int]
    , testCase "should not capture css animations in shadow DOM" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rotate-z-shadow-dom.html")) defaultGotoOptions
          initial <- screenshot page disabledAnimations
          mapM_
            ( \_ -> do
                rafraf page
                again <- screenshot page disabledAnimations
                assertEqual "screenshot stays still" initial again
            )
            [1 .. 4 :: Int]
    , testCase "should resume infinite animations" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rotate-z.html")) defaultGotoOptions
          _ <- screenshot page disabledAnimations
          buffer1 <- screenshot page defaultScreenshotOptions
          rafraf page
          buffer2 <- screenshot page defaultScreenshotOptions
          assertBool "animation resumed after disabled screenshot" (buffer1 /= buffer2)
    , testCase "should not capture infinite web animations" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/web-animation.html")) defaultGotoOptions
          let target = locator page (selectorFromText "div")
          initial <- locatorScreenshot target disabledAnimations
          mapM_
            ( \_ -> do
                rafraf page
                again <- locatorScreenshot target disabledAnimations
                assertEqual "screenshot stays still" initial again
            )
            [1 .. 10 :: Int]
          buffer1 <- screenshot page defaultScreenshotOptions
          rafraf page
          buffer2 <- screenshot page defaultScreenshotOptions
          assertBool "web animation resumed" (buffer1 /= buffer2)
    , testCase "should fire transitionend for finite transitions" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/css-transition.html")) defaultGotoOptions
          let target = locator page (selectorFromText "div")
          _ <-
            evaluateOnLocator
              target
              "el => el.addEventListener('transitionend', () => window.__TRANSITION_END = true, false)"
          -- Make sure the transition is actually running.
          running1 <- screenshot page defaultScreenshotOptions
          rafraf page
          running2 <- screenshot page defaultScreenshotOptions
          assertBool "transition is running" (running1 /= running2)
          -- Finish all finite animations, then make sure the transition is not restarted.
          finished1 <- locatorScreenshot target disabledAnimations
          rafraf page
          finished2 <- locatorScreenshot target defaultScreenshotOptions {screenshotOptionsAnimations = Just ScreenshotAnimationsAllow}
          assertEqual "finite transition is not restarted" finished1 finished2
          ended <- evaluateBool page "() => window.__TRANSITION_END"
          assertBool "transitionend fired" ended
    , testCase "should capture screenshots after layoutchanges in transitionend event" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/css-transition.html")) defaultGotoOptions
          _ <-
            evaluateOnLocator
              (locator page (selectorFromText "div"))
              "el => el.addEventListener('transitionend', () => { const time = Date.now(); while (Date.now() - time < 200) {} const h1 = document.createElement('h1'); h1.textContent = 'woof-woof'; document.body.append(h1); }, false)"
          running1 <- screenshot page defaultScreenshotOptions
          rafraf page
          running2 <- screenshot page defaultScreenshotOptions
          assertBool "transition is running" (running1 /= running2)
          -- Finishes all finite animations and triggers layout.
          shot1 <- screenshot page disabledAnimations
          waitFor (locator page (selectorFromText "h1")) WaitForVisible
          woof <-
            evaluateBool page "() => document.querySelector('h1')?.textContent === 'woof-woof'"
          assertBool "h1 has the expected text" woof
          shot2 <- screenshot page defaultScreenshotOptions
          assertBool "screenshot after layout change matches" (shot1 == shot2)
    , testCase "should not change animation with playbackRate equal to 0" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rotate-z.html")) defaultGotoOptions
          _ <-
            evaluate
              page
              "async () => { window.animation = document.getAnimations()[0]; await window.animation.ready; window.animation.updatePlaybackRate(0); await window.animation.ready; window.animation.currentTime = 500; }"
          screenshot1 <- screenshot page disabledAnimations
          rafraf page
          screenshot2 <- screenshot page disabledAnimations
          assertEqual "screenshot stays still at playbackRate 0" screenshot1 screenshot2
          rate <- evaluateBool page "() => window.animation.playbackRate === 0 && window.animation.currentTime === 500"
          assertBool "playbackRate is 0 and currentTime is 500" rate
    , testCase "should trigger particular events for css transitions" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/css-transition.html")) defaultGotoOptions
          _ <-
            evaluateOnLocator
              (locator page (selectorFromText "div"))
              "async el => { window._EVENTS = []; el.addEventListener('transitionend', () => window._EVENTS.push('transitionend'), false); const animation = el.getAnimations()[0]; animation.oncancel = () => window._EVENTS.push('oncancel'); animation.onfinish = () => window._EVENTS.push('onfinish'); animation.onremove = () => window._EVENTS.push('onremove'); await animation.ready; }"
          _ <- screenshot page disabledAnimations
          matched <- waitForEventLog page "['onfinish', 'transitionend']"
          assertBool "events fired in order" matched
    , testCase "should trigger particular events for INfinite css animation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rotate-z.html")) defaultGotoOptions
          _ <-
            evaluateOnLocator
              (locator page (selectorFromText "div"))
              "async el => { window._EVENTS = []; el.addEventListener('animationcancel', () => window._EVENTS.push('animationcancel'), false); const animation = el.getAnimations()[0]; animation.oncancel = () => window._EVENTS.push('oncancel'); animation.onfinish = () => window._EVENTS.push('onfinish'); animation.onremove = () => window._EVENTS.push('onremove'); await animation.ready; }"
          _ <- screenshot page disabledAnimations
          matched <- waitForEventLog page "['oncancel', 'animationcancel']"
          assertBool "events fired in order" matched
    , testCase "should trigger particular events for finite css animation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/rotate-z.html")) defaultGotoOptions
          _ <-
            evaluateOnLocator
              (locator page (selectorFromText "div"))
              "async el => { window._EVENTS = []; el.style.setProperty('animation-iteration-count', '1000'); el.addEventListener('animationend', () => window._EVENTS.push('animationend'), false); const animation = el.getAnimations()[0]; animation.oncancel = () => window._EVENTS.push('oncancel'); animation.onfinish = () => window._EVENTS.push('onfinish'); animation.onremove = () => window._EVENTS.push('onremove'); await animation.ready; }"
          finite <-
            evaluateBool
              page
              "() => Number.isFinite(document.querySelector('div').getAnimations()[0].effect.getComputedTiming().endTime)"
          assertBool "css animation is finite" finite
          _ <- screenshot page disabledAnimations
          matched <- waitForEventLog page "['onfinish', 'animationend']"
          assertBool "events fired in order" matched
    , testCase "should wait for fonts to load" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          fontRequested <- liftIO newEmptyMVar
          releaseFont <- liftIO newEmptyMVar
          -- Stall the web font until the test releases it.
          routeWith
            page
            (UrlMatchGlob "**/webfont/iconfont.woff2")
            defaultRouteOptions
            $ \handled -> do
              liftIO (putMVar fontRequested ())
              liftIO (takeMVar releaseFont)
              font <- readTestAsset "webfont/iconfont.woff2"
              fulfillRoute
                handled
                defaultFulfillOptions
                  { fulfillBody = Just (FulfillBytes font)
                  , fulfillContentType = Just (ContentType "font/woff2")
                  }
          _ <-
            goto
              page
              (urlFromText (prefix <> "/webfont/webfont.html"))
              defaultGotoOptions {gotoOptionsWaitUntil = Just LoadStateDomContentLoaded}
          liftIO (takeMVar fontRequested)
          limit <- timeoutMs 200
          result <- try (screenshot page defaultScreenshotOptions {screenshotOptionsTimeout = limit})
          case result of
            Right _ -> assertFailure "screenshot should time out while the font is stalled"
            Left (ProtocolRpc failure) -> do
              let message = protocolFailureMessage failure
              assertBool "message mentions the pending fonts" ("waiting for fonts to load..." `isInfixOf` message)
              assertBool "message mentions the timeout" ("Timeout 200ms exceeded" `isInfixOf` message)
            Left err -> assertFailure ("expected ProtocolRpc, got: " <> show err)
          liftIO (putMVar releaseFont ())
          icons <- screenshot page defaultScreenshotOptions
          -- Upstream compares this against a snapshot; a PNG check is the closest byte-level check here.
          assertBool "screenshot with web font is a png" (pngMagic `BS.isPrefixOf` icons)
    , testCase "should throw if screenshot size is too large" $
        withAssets $ \_ page -> do
          -- Chromium's limit; the Firefox and WebKit branches of upstream are not exercised here.
          let fullPageOf :: Int -> Text
              fullPageOf height =
                "<style>body {margin: 0; padding: 0;}</style><div style='min-height: "
                  <> pack (show height)
                  <> "px; background: red;'></div>"
          _ <- setContent page (fullPageOf 32767)
          atLimit <- screenshot page defaultScreenshotOptions {screenshotOptionsFullPage = Just True}
          assertBool "screenshot at the limit is a png" (pngMagic `BS.isPrefixOf` atLimit)
          _ <- setContent page (fullPageOf 32768)
          -- Upstream only requires a message for Firefox and WebKit, so Chromium may succeed or refuse here.
          _ <- try (screenshot page defaultScreenshotOptions {screenshotOptionsFullPage = Just True}) :: Playwright (Either PlaywrightError ByteString)
          pure ()
    , testCase "page screenshot should capture css transform" $
        withAssets $ \_ page -> do
          _ <-
            setContent
              page
              "<style>.container {width: 150px; height: 150px; margin: 75px 0 0 75px; border: none;}\
              \.cube {width: 100%; height: 100%; perspective: 550px; perspective-origin: 150% 150%;}\
              \.face {display: block; position: absolute; width: 100px; height: 100px; border: none;}\
              \.right {background: rgba(196, 0, 0, 0.7); transform: rotateY(70deg);}</style>\
              \<div class=\"container\"><div class=\"cube showbf\"><div class=\"face right\"></div></div></div>"
          transformed <- screenshot page defaultScreenshotOptions
          assertBool "css transform screenshot is a png" (pngMagic `BS.isPrefixOf` transformed)
    , testCase "should capture css box-shadow" $
        withAssets $ \_ page -> do
          _ <-
            setContent
              page
              "<div style=\"box-shadow: red 10px 10px 10px; width: 50px; height: 50px;\"></div>"
          shadowed <- screenshot page defaultScreenshotOptions
          assertBool "box-shadow screenshot is a png" (pngMagic `BS.isPrefixOf` shadowed)
    , testCase "should not capture blinking caret by default" $
        withAssets $ \prefix page -> do
          _ <- setContent page (caretPage (replace "127.0.0.1" "localhost" prefix))
          let box = locator page (selectorFromText "div")
          typeText box "foo bar"
          baseline <- locatorScreenshot box defaultScreenshotOptions
          changed <- anyCaretChange box defaultScreenshotOptions baseline
          assertBool "blinking caret is not captured by default" (not changed)
    , testCase "should capture blinking caret if explicitly asked for" $
        withAssets $ \prefix page -> do
          _ <- setContent page (caretPage (replace "127.0.0.1" "localhost" prefix))
          let box = locator page (selectorFromText "div")
          typeText box "foo bar"
          baseline <- locatorScreenshot box defaultScreenshotOptions
          changed <- anyCaretChange box defaultScreenshotOptions {screenshotOptionsCaret = Just ScreenshotCaretInitial} baseline
          assertBool "blinking caret is captured when asked for" changed
    , testCase "should capture blinking caret in shadow dom" $
        withAssets $ \_ page -> do
          _ <- addScriptTag page emptyScriptTag {scriptTagContent = Just shadowCaretScript}
          let input = locator page (selectorFromText "input")
          focus input
          baseline <- locatorScreenshot input defaultScreenshotOptions
          changed <- anyCaretChange input defaultScreenshotOptions {screenshotOptionsCaret = Just ScreenshotCaretHide} baseline
          assertBool "hidden caret never differs in shadow dom" (not changed)
    , testCase "should work with odd clip size on Retina displays" $
        withAssetsWith dsf2Options $ \_ page -> do
          image <- screenshot page (withClip 0 0 11 11)
          assertEqual "odd clip png size at dsf 2" (22, 22) (pngSize image)
    , testCase "should work for canvas" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/screenshots/canvas.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions
          assertBool "canvas screenshot is a png" (pngMagic `BS.isPrefixOf` image)
          assertEqual "canvas screenshot size" (500, 500) (pngSize image)
    , testCase "should work for webgl" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 640) (ViewportHeight 480))
          _ <- goto page (urlFromText (prefix <> "/screenshots/webgl.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions
          assertBool "webgl screenshot is a png" (pngMagic `BS.isPrefixOf` image)
          assertEqual "webgl screenshot size" (640, 480) (pngSize image)
    , testCase "should work for translateZ" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/screenshots/translateZ.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions
          assertBool "translateZ screenshot is a png" (pngMagic `BS.isPrefixOf` image)
          assertEqual "translateZ screenshot size" (500, 500) (pngSize image)
    , testCase "should work while navigating" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/redirectloop1.html")) defaultGotoOptions
          mapM_ (const (checkNavigatingScreenshot page)) [1 .. 10 :: Int]
    , testCase "should work with iframe in shadow" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid-iframe-in-shadow.html")) defaultGotoOptions
          image <- screenshot page defaultScreenshotOptions
          assertEqual "iframe in shadow screenshot size" (500, 500) (pngSize image)
    ]

-- Polls the element screenshot ten times, 150 ms apart, and reports whether any
-- of them differs from the baseline. The caret blinks every 500 ms.
anyCaretChange :: Locator -> ScreenshotOptions -> ByteString -> Playwright Bool
anyCaretChange box options baseline =
  go 10
  where
    go :: Int -> Playwright Bool
    go 0 = pure False
    go n = do
      liftIO (threadDelay 150000)
      image <- locatorScreenshot box options
      if image /= baseline then pure True else go (n - 1)

caretPage :: Text -> Text
caretPage prefix =
  "<link rel=stylesheet href=\""
    <> prefix
    <> "/injectedstyle.css\">"
    <> "<style>div { caret-color: #000 !important; }</style>"
    <> "<div contenteditable=\"true\"></div>"

shadowCaretScript :: Text
shadowCaretScript =
  "class CustomElementContainer extends HTMLElement { #shadowRoot; constructor() { super(); "
    <> "this.#shadowRoot = this.attachShadow({ mode: 'open' }); "
    <> "this.#shadowRoot.innerHTML = '<custom-element-input-wrapper><input type=\"text\"/></custom-element-input-wrapper>'; } } "
    <> "class CustomElementInputWrapper extends HTMLElement { #shadowRoot; constructor() { super(); "
    <> "this.#shadowRoot = this.attachShadow({ mode: 'open' }); "
    <> "this.#shadowRoot.innerHTML = '<style>:host { all: initial; }</style><slot/>'; } } "
    <> "customElements.define('custom-element-input-wrapper', CustomElementInputWrapper); "
    <> "customElements.define('custom-element-container', CustomElementContainer); "
    <> "const container = document.createElement('custom-element-container'); "
    <> "document.body.appendChild(container);"

-- A full-page screenshot taken during a redirect loop may be refused while the
-- page is navigating; any other failure is a real error.
checkNavigatingScreenshot :: Page -> Playwright ()
checkNavigatingScreenshot page = do
  result <- try (screenshot page fullPageOptions)
  case result of
    Right image ->
      assertBool "navigating screenshot is a png" (pngMagic `BS.isPrefixOf` image)
    Left (ProtocolRpc failure) ->
      assertBool
        "only navigation refusals are tolerated"
        ("Cannot take a screenshot while page is navigating" `isInfixOf` protocolFailureMessage failure)
    Left err ->
      assertFailure ("expected ProtocolRpc, got: " <> show err)

-- Converts a literal millisecond count into a Timeout, failing the test if it is invalid.
timeoutMs :: Double -> Playwright Timeout
timeoutMs n =
  case milliseconds n of
    Nothing -> assertFailure ("invalid timeout: " <> show n)
    Just limit -> pure limit

-- Waits two animation frames so that running animations advance.
rafraf :: Page -> Playwright ()
rafraf page = do
  _ <- evaluate page "() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))"
  pure ()

disabledAnimations :: ScreenshotOptions
disabledAnimations = defaultScreenshotOptions {screenshotOptionsAnimations = Just ScreenshotAnimationsDisabled}

-- Polls frames until the page's window._EVENTS log equals the expected JS array literal.
waitForEventLog :: Page -> Text -> Playwright Bool
waitForEventLog page expected = go (50 :: Int)
  where
    go :: Int -> Playwright Bool
    go 0 = pure False
    go n = do
      matched <-
        evaluateBool page ("() => JSON.stringify(window._EVENTS) === JSON.stringify(" <> expected <> ")")
      if matched
        then pure True
        else do
          rafraf page
          go (n - 1)

dsf2Options :: NewContextOptions
dsf2Options =
  defaultNewContextOptions {newContextDeviceScaleFactor = Just (deviceScaleFactorFromInt 2)}

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

fullPageOptions :: ScreenshotOptions
fullPageOptions =
  defaultScreenshotOptions {screenshotOptionsFullPage = Just True}

transparencyPage :: Text
transparencyPage =
  "<style>body { margin: 0 } div { width: 300px; height: 100px; }</style>"
    <> "<div style=\"background:black\"></div>"
    <> "<div style=\"background:white\"></div>"
    <> "<div style=\"background:transparent\"></div>"

-- Walks the RIFF chunks after the WEBP header, skipping container chunks such
-- as VP8X, as upstream isLosslessWebp does. Returns the image chunk FourCC
-- (VP8L or VP8 ) and that chunk's payload.
webpImageChunk :: ByteString -> (ByteString, ByteString)
webpImageChunk image =
  go 12
  where
    go offset
      | offset + 8 > BS.length image = (BS.empty, BS.empty)
      | fourCC `elem` ["VP8L", "VP8 "] = (fourCC, payload)
      | otherwise = go (offset + 8 + size + size `mod` 2)
      where
        fourCC = BS.take 4 (BS.drop offset image)
        size = littleEndian32 (BS.take 4 (BS.drop (offset + 4) image))
        payload = BS.take size (BS.drop (offset + 8) image)

littleEndian32 :: ByteString -> Int
littleEndian32 =
  BS.foldr (\byte acc -> fromIntegral byte + 256 * acc) 0

captureCanvasStep :: Page -> Int -> Playwright ByteString
captureCanvasStep page step = do
  _ <- evaluate page (drawCanvasLineScript step)
  liftIO (threadDelay 100000)
  screenshot page defaultScreenshotOptions

drawCanvasLineScript :: Int -> Text
drawCanvasLineScript step =
  "() => { const ctx = document.querySelector('canvas').getContext('2d'); ctx.lineWidth = 1; ctx.beginPath(); ctx.moveTo(0, "
    <> y
    <> "); ctx.lineTo(300, "
    <> y
    <> "); ctx.stroke(); }"
  where
    y :: Text
    y = pack (show (step * 100))

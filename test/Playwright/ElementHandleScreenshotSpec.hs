module Playwright.ElementHandleScreenshotSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as BS
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( BindingName (BindingName)
  , Page
  , Playwright
  , ScreenshotOptions
      ( screenshotOptionsPath
      , screenshotOptionsTimeout
      , screenshotOptionsType
      )
  , ScreenshotType (ScreenshotJpeg)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , defaultGotoOptions
  , defaultScreenshotOptions
  , evaluate
  , evaluateNumber
  , evaluateOnElement
  , exposeFunction
  , goto
  , milliseconds
  , querySelector
  , querySelectorAll
  , screenshotHandle
  , screenshotHandleWith
  , selectorFromText
  , setContent
  , setViewportSize
  , undefinedArgument
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( requireHandle
  , withAssets
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
  , poll
  , wait
  , withAsync
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
    "elementhandle-screenshot"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          image <- screenshotHandle handle
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
          assertBool "non-empty" (BS.length image > 100)
    , testCase "should work when main world busts JSON.stringify" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- evaluate page "() => { window.scrollBy(50, 100); JSON.stringify = () => undefined; }"
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          image <- screenshotHandle handle
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "should take into account padding and border" $
        withAssets $ \_ page -> do
          setContent
            page
            "<div style=\"height: 14px\">oooo</div><style>div { border: 2px solid blue; background: green; width: 50px; height: 50px; }</style><div id=\"d\"></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div#d")
          image <- screenshotHandle handle
          assertEqual "size" (54, 54) (pngSize image)
    , testCase "should capture full element when larger than viewport in parallel" $
        withAssets $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          setContent
            page
            "<div style=\"height: 14px\">oooo</div><style>div.to-screenshot { border: 1px solid blue; width: 600px; height: 600px; margin-left: 50px; } ::-webkit-scrollbar{ display: none; }</style><div class=\"to-screenshot\"></div><div class=\"to-screenshot\"></div><div class=\"to-screenshot\"></div>"
          handles <- querySelectorAll page (selectorFromText "div.to-screenshot")
          assertEqual "count" 3 (length handles)
          images <- mapConcurrently screenshotHandle handles
          case images of
            [_, _, third] -> assertEqual "size" (602, 602) (pngSize third)
            _ -> assertFailure "expected three screenshots"
          assertEqual "innerWidth" 500 =<< evaluateNumber page "() => window.innerWidth"
          assertEqual "innerHeight" 500 =<< evaluateNumber page "() => window.innerHeight"
    , testCase "should capture full element when larger than viewport" $
        withAssets $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          setContent
            page
            "<div style=\"height: 14px\">oooo</div><style>div.to-screenshot { border: 1px solid blue; width: 600px; height: 600px; margin-left: 50px; } ::-webkit-scrollbar{ display: none; }</style><div class=\"to-screenshot\"></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div.to-screenshot")
          image <- screenshotHandle handle
          assertEqual "size" (602, 602) (pngSize image)
    , testCase "should scroll element into view" $
        withAssets $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          setContent page (scrollIntoViewHtml "1500px")
          handle <- requireHandle =<< querySelector page (selectorFromText "div.to-screenshot")
          image <- screenshotHandle handle
          assertEqual "size" (54, 54) (pngSize image)
    , testCase "should scroll 15000px into view" $
        withAssets $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          setContent page (scrollIntoViewHtml "15000px")
          handle <- requireHandle =<< querySelector page (selectorFromText "div.to-screenshot")
          image <- screenshotHandle handle
          assertEqual "size" (54, 54) (pngSize image)
    , testCase "should work with a rotated element" $
        withAssets $ \_ page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          setContent
            page
            "<div style=\"position:absolute; top: 100px; left: 100px; width: 100px; height: 100px; background: green; transform: rotateZ(200deg);\">&nbsp;</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          image <- screenshotHandle handle
          let (width, height) = pngSize image
          assertBool "width" (width >= 120 && width <= 135)
          assertBool "height" (height >= 120 && height <= 135)
    , testCase "should fail to screenshot a detached element" $
        withAssets $ \_ page -> do
          setContent page "<h1>remove this</h1>"
          handle <- requireHandle =<< querySelector page (selectorFromText "h1")
          _ <- evaluateOnElement handle "e => e.remove()"
          result <- try (screenshotHandle handle)
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "detached message"
                ("Element is not attached to the DOM" `isInfixOf` protocolFailureMessage failure)
            Left err -> assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ -> assertFailure "expected the screenshot to fail"
    , testCase "should timeout waiting for visible" $
        withAssets $ \_ page -> do
          setContent page "<div style=\"width: 50px; height: 0\"></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          case milliseconds 3000 of
            Nothing -> assertFailure "3000ms should be a valid timeout"
            Just limit -> do
              result <-
                try
                  (screenshotHandleWith handle [] defaultScreenshotOptions {screenshotOptionsTimeout = limit})
              case result of
                Left (ProtocolRpc failure) -> do
                  -- The TypeScript client prefixes "elementHandle.screenshot: " to its
                  -- errors; this library reports the driver's message as-is.
                  assertBool
                    ("timeout message, got: " <> show (protocolFailureMessage failure))
                    ("Timeout 3000ms exceeded" `isInfixOf` protocolFailureMessage failure)
                  assertBool
                    ("visibility message, got: " <> show (protocolFailureMessage failure))
                    ("element is not visible" `isInfixOf` protocolFailureMessage failure)
                Left err -> assertFailure ("expected ProtocolRpc, got: " <> show err)
                Right _ -> assertFailure "expected the screenshot to time out"
    , testCase "should wait for visible" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- evaluate page "() => window.scrollBy(50, 100)"
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          _ <- evaluateOnElement handle "e => e.style.visibility = 'hidden'"
          withAsync (screenshotHandle handle) $ \shot -> do
            rafraf page 10
            pending <- poll shot
            assertEqual "pending" True (isNothing pending)
            _ <- evaluateOnElement handle "e => e.style.visibility = 'visible'"
            image <- wait shot
            assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "should work for an element with fractional dimensions" $
        withAssets $ \_ page -> do
          setContent page "<div style=\"width:48.51px;height:19.8px;border:1px solid black;\"></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          image <- screenshotHandle handle
          let (width, height) = pngSize image
          assertBool "width" (width >= 50 && width <= 52)
          assertBool "height" (height >= 20 && height <= 23)
    , testCase "should work for an element with an offset" $
        withAssets $ \_ page -> do
          setContent
            page
            "<div style=\"position:absolute; top: 10.3px; left: 20.4px;width:50.3px;height:20.2px;border:1px solid black;\"></div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          image <- screenshotHandle handle
          let (width, height) = pngSize image
          assertBool "width" (width >= 50 && width <= 54)
          assertBool "height" (height >= 20 && height <= 24)
    , testCase "should wait for element to stop moving" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          _ <- evaluateOnElement handle "e => e.classList.add('animation')"
          rafraf page 1
          image <- screenshotHandle handle
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "should take screenshot of disabled button" $
        withAssets $ \_ page -> do
          setContent page "<button disabled>Click me</button>"
          handle <- requireHandle =<< querySelector page (selectorFromText "button")
          image <- screenshotHandle handle
          assertBool "png signature" (pngMagic `BS.isPrefixOf` image)
    , testCase "path option should create subdirectories" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- evaluate page "() => window.scrollBy(50, 100)"
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          withSystemTempDirectory "screenshot" $ \dir -> do
            let outputPath = dir <> "/these/are/directories/screenshot.png" :: FilePath
            _ <-
              screenshotHandleWith
                handle
                []
                defaultScreenshotOptions {screenshotOptionsPath = Just outputPath}
            saved <- liftIO (BS.readFile outputPath)
            assertBool "png signature" (pngMagic `BS.isPrefixOf` saved)
    , testCase "should prefer type over extension" $
        withAssets $ \prefix page -> do
          setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 500))
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          _ <- evaluate page "() => window.scrollBy(50, 100)"
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          withSystemTempDirectory "screenshot" $ \dir -> do
            image <-
              screenshotHandleWith
                handle
                []
                defaultScreenshotOptions
                  { screenshotOptionsPath = Just (dir <> "/file.png")
                  , screenshotOptionsType = Just ScreenshotJpeg
                  }
            assertBool "jpeg signature" (jpegMagic `BS.isPrefixOf` image)
    , testCase "should not issue resize event" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/grid.html")) defaultGotoOptions
          flag <- newIORef False
          _ <-
            exposeFunction
              page
              (BindingName "resize")
              (\_ -> liftIO (writeIORef flag True) >> pure undefinedArgument)
          _ <- evaluate page "() => { window.addEventListener('resize', () => window.resize()); }"
          handle <- requireHandle =<< querySelector page (selectorFromText ".box:nth-of-type(3)")
          _ <- screenshotHandle handle
          triggered <- readIORef flag
          assertEqual "resize triggered" False triggered
    ]

-- Reproduces the upstream helper: two animation frames per iteration, so the page settles.
rafraf :: Page -> Int -> Playwright ()
rafraf page count =
  forM_ [1 .. count] $ \(_ :: Int) -> do
    _ <-
      evaluate
        page
        "async () => { await new Promise(f => window.requestAnimationFrame(() => window.requestAnimationFrame(f))); }"
    pure ()

-- A div that sits below a tall spacer, so the screenshot target is off-screen.
scrollIntoViewHtml :: Text -> Text
scrollIntoViewHtml height =
  "<div style=\"height: 14px\">oooo</div><style>div.above { border: 2px solid blue; background: red; height: "
    <> height
    <> "; } div.to-screenshot { border: 2px solid blue; background: green; width: 50px; height: 50px; }</style><div class=\"above\"></div><div class=\"to-screenshot\"></div>"

pngMagic :: ByteString
pngMagic =
  "\137PNG\r\n\26\n"

jpegMagic :: ByteString
jpegMagic =
  "\255\216\255"

-- Width and height come from the IHDR chunk: big-endian u32 at bytes 16-19 and 20-23.
pngSize :: ByteString -> (Int, Int)
pngSize image =
  (bigEndian32 (BS.take 4 (BS.drop 16 image)), bigEndian32 (BS.take 4 (BS.drop 20 image)))

bigEndian32 :: ByteString -> Int
bigEndian32 =
  BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

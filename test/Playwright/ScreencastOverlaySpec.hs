module Playwright.ScreencastOverlaySpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad
  ( unless
  , void
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Codec.Compression.Zlib qualified as Zlib
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Data.Word
  ( Word8
  )
import Playwright
  ( AriaRole (AriaRole)
  , GetByRoleOptions (getByRoleName)
  , Page
  , Playwright
  , ScreencastOverlayOptions (screencastOverlayOptionsDuration)
  , click
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultScreenshotOptions
  , defaultScreencastOverlayOptions
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , getByRole
  , goto
  , isHidden
  , isVisible
  , locatorCount
  , reload
  , screenshot
  , screencastHideOverlays
  , screencastOverlayDispose
  , screencastShowOverlay
  , screencastShowOverlays
  , setContent
  , urlFromText
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , locate
  , withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- | Polls a condition for up to about five seconds, as upstream's @expect@ does.
eventually :: Playwright Bool -> Playwright ()
eventually check = go (50 :: Int)
  where
    go :: Int -> Playwright ()
    go 0 = assertFailure "condition was not met in time"
    go n = do
      ok <- check
      unless ok $ do
        liftIO (threadDelay 100000)
        go (n - 1)

-- | Whether exactly @n@ elements match the selector.
countIs :: Page -> Text -> Int -> Playwright Bool
countIs page selector n = (== n) <$> locatorCount (locate page selector)

-- | Opens the empty page fixture.
gotoEmpty :: Text -> Page -> Playwright ()
gotoEmpty prefix page =
  void (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)

-- | Whether the element's text equals the expected value.
textIs :: Page -> Text -> Text -> Playwright Bool
textIs page selector expected =
  (== expected) <$> evaluateOnLocatorText (locate page selector) "el => el.innerText"

-- | The RGB channels of one pixel of a PNG screenshot. Decodes an 8-bit,
-- non-interlaced RGB or RGBA PNG, which is all Chromium's screenshots are.
pixelAt :: BS.ByteString -> Int -> Int -> [Word8]
pixelAt png x y = take 3 (BS.unpack (BS.drop (x * bpp) (rows !! y)))
  where
    (bpp, rows) = pngRows png

pngRows :: BS.ByteString -> (Int, [BS.ByteString])
pngRows png = (bpp, unfilterRows (width * bpp) bpp (BSL.toStrict (Zlib.decompress (BSL.fromStrict idat))))
  where
    chunks = pngChunks (BS.drop 8 png)
    ihdr = fromMaybe BS.empty (lookup "IHDR" chunks)
    width = be32 (BS.take 4 ihdr)
    bpp :: Int
    bpp = if BS.index ihdr 9 == 6 then 4 else 3
    idat = BS.concat [dat | (typ, dat) <- chunks, typ == "IDAT"]

pngChunks :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
pngChunks bs
  | BS.length bs < 12 = []
  | otherwise =
      let len = be32 (BS.take 4 bs)
          typ = BS.take 4 (BS.drop 4 bs)
          dat = BS.take len (BS.drop 8 bs)
       in (typ, dat) : pngChunks (BS.drop (12 + len) bs)

be32 :: BS.ByteString -> Int
be32 = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

-- | Reverses the PNG scanline filters, returning each row without its filter byte.
unfilterRows :: Int -> Int -> BS.ByteString -> [BS.ByteString]
unfilterRows stride bpp = go (BS.replicate stride 0)
  where
    go prev bs
      | BS.length bs < stride + 1 = []
      | otherwise =
          let line = unfilterRow bpp (BS.head bs) prev (BS.take stride (BS.drop 1 bs))
           in line : go line (BS.drop (stride + 1) bs)

unfilterRow :: Int -> Word8 -> BS.ByteString -> BS.ByteString -> BS.ByteString
unfilterRow bpp filterType prev raw = BS.pack (reverse out)
  where
    (out, _) = BS.foldl' step ([], 0 :: Int) raw
    step (acc, x) byte =
      let a = if x >= bpp then acc !! (bpp - 1) else 0
          b = BS.index prev x
          c = if x >= bpp then BS.index prev (x - bpp) else 0
          recon = case filterType of
            0 -> byte
            1 -> byte + a
            2 -> byte + b
            3 -> byte + fromIntegral ((fromIntegral a + fromIntegral b :: Int) `div` 2)
            _ -> byte + paeth a b c
       in (recon : acc, x + 1)

paeth :: Word8 -> Word8 -> Word8 -> Word8
paeth a b c
  | pa <= pb && pa <= pc = a
  | pb <= pc = b
  | otherwise = c
  where
    p = fromIntegral a + fromIntegral b - fromIntegral c :: Int
    dist :: Word8 -> Int
    dist v = abs (p - fromIntegral v)
    pa = dist a
    pb = dist b
    pc = dist c

-- | Whether the pixel at (x, y) of the page's current screenshot has this RGB.
pixelIs :: Page -> Int -> Int -> [Word8] -> Playwright Bool
pixelIs page x y expected = do
  png <- screenshot page defaultScreenshotOptions
  pure (pixelAt png x y == expected)

-- | Top-layer pages: a full-page dialog or popover opened by the "Open" button.
topLayerPages :: [(String, Text)]
topLayerPages =
  [ ("dialog", "<dialog id='target' style='position: fixed; inset: 0; margin: 0; width: 100vw; height: 100vh; padding: 0; border: 0; background: rgb(200, 200, 200);'></dialog><button onclick='target.showModal()'>Open</button>")
  , ("popover", "<div id='target' popover='manual' style='inset: 0; margin: 0; width: 100%; height: 100%; max-width: none; max-height: none; padding: 0; border: 0; background: rgb(200, 200, 200);'></div><button onclick='target.showPopover()'>Open</button>")
  ]

redOverlay :: Text
redOverlay = "<div style='position: absolute; top: 50px; left: 50px; width: 200px; height: 100px; background: rgb(255, 0, 0);'></div>"

-- | Clicks the page's "Open" button.
clickOpen :: Page -> Playwright ()
clickOpen page =
  click (getByRole page (AriaRole "button") defaultGetByRoleOptions {getByRoleName = Just "Open"})

topLayerTests :: [TestTree]
topLayerTests =
  concat
    [ [ testCase ("should show overlay above " <> kind <> " opened before it") $
          withAssets $ \_ page -> do
            setContent page html
            disposable <- screencastShowOverlay page "<div></div>" defaultScreencastOverlayOptions
            screencastOverlayDispose disposable
            clickOpen page
            eventually (pixelIs page 100 100 [200, 200, 200])
            _ <- screencastShowOverlay page redOverlay defaultScreencastOverlayOptions
            eventually (pixelIs page 100 100 [255, 0, 0])
      , testCase ("should keep overlay above " <> kind <> " opened after it") $
          withAssets $ \_ page -> do
            setContent page html
            _ <- screencastShowOverlay page redOverlay defaultScreencastOverlayOptions
            eventually (pixelIs page 100 100 [255, 0, 0])
            clickOpen page
            eventually (pixelIs page 100 100 [255, 0, 0])
      ]
    | (kind, html) <- topLayerPages
    ]

tests :: TestTree
tests =
  testGroup
    "screencast-overlay"
    $ [ testCase "should add and remove overlay" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          disposable <- screencastShowOverlay page "<div id=\"my-overlay\">Hello Overlay</div>" defaultScreencastOverlayOptions
          eventually (isVisible (locate page "x-pw-user-overlays"))
          eventually (countIs page ".x-pw-user-overlay" 1)
          eventually (textIs page "#my-overlay" "Hello Overlay")
          screencastOverlayDispose disposable
          eventually (countIs page ".x-pw-user-overlay" 0)
    , testCase "should add multiple overlays" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          d1 <- screencastShowOverlay page "<div id=\"overlay-1\">First</div>" defaultScreencastOverlayOptions
          d2 <- screencastShowOverlay page "<div id=\"overlay-2\">Second</div>" defaultScreencastOverlayOptions
          eventually (countIs page ".x-pw-user-overlay" 2)
          eventually (textIs page "#overlay-1" "First")
          eventually (textIs page "#overlay-2" "Second")
          screencastOverlayDispose d1
          eventually (countIs page ".x-pw-user-overlay" 1)
          eventually (textIs page "#overlay-2" "Second")
          screencastOverlayDispose d2
          eventually (countIs page ".x-pw-user-overlay" 0)
    , testCase "should hide and show overlays" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          _ <- screencastShowOverlay page "<div id=\"my-overlay\">Visible</div>" defaultScreencastOverlayOptions
          eventually (isVisible (locate page "x-pw-user-overlays"))
          screencastHideOverlays page
          eventually (isHidden (locate page "x-pw-user-overlays"))
          screencastShowOverlays page
          eventually (isVisible (locate page "x-pw-user-overlays"))
          eventually (textIs page "#my-overlay" "Visible")
    , testCase "should survive navigation" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          _ <- screencastShowOverlay page "<div id=\"persistent\">Survives Reload</div>" defaultScreencastOverlayOptions
          eventually (textIs page "#persistent" "Survives Reload")
          gotoEmpty prefix page
          eventually (textIs page "#persistent" "Survives Reload")
          void (reload page)
          eventually (textIs page "#persistent" "Survives Reload")
    , testCase "should remove overlay and not restore after navigation" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          disposable <- screencastShowOverlay page "<div id=\"temp\">Temporary</div>" defaultScreencastOverlayOptions
          eventually (textIs page "#temp" "Temporary")
          screencastOverlayDispose disposable
          eventually (countIs page ".x-pw-user-overlay" 0)
          void (reload page)
          eventually (countIs page ".x-pw-user-overlay" 0)
    , testCase "should sanitize scripts from overlay html" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          _ <- screencastShowOverlay page "<div id=\"safe\">Safe</div><script>window.__injected = true</script>" defaultScreencastOverlayOptions
          eventually (textIs page "#safe" "Safe")
          injected <- evaluateOnLocatorBool (locate page "body") "el => window.__injected === true"
          assertBool "overlay script did not run" (not injected)
    , testCase "should strip event handlers from overlay html" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          _ <- screencastShowOverlay page "<div id=\"clean\" onclick=\"window.__clicked=true\">Click me</div>" defaultScreencastOverlayOptions
          eventually (textIs page "#clean" "Click me")
          hasOnclick <- evaluateOnLocatorBool (locate page "#clean") "el => el.hasAttribute('onclick')"
          assertBool "onclick was stripped" (not hasOnclick)
    , testCase "should auto-remove overlay after timeout" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          _ <- screencastShowOverlay page "<div id=\"timed\">Temporary</div>" defaultScreencastOverlayOptions {screencastOverlayOptionsDuration = Just 1}
          eventually (countIs page ".x-pw-user-overlay" 0)
    , testCase "should allow styles in overlay html" $
        withAssets $ \prefix page -> do
          gotoEmpty prefix page
          _ <- screencastShowOverlay page "<div id=\"styled\" style=\"color: red; font-size: 20px;\">Styled</div>" defaultScreencastOverlayOptions
          eventually (textIs page "#styled" "Styled")
          color <- evaluateOnLocatorText (locate page "#styled") "el => getComputedStyle(el).color"
          assertEqual "overlay text colour" "rgb(255, 0, 0)" color
    ]
      ++ topLayerTests

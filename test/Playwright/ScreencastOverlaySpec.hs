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
import Data.Text
  ( Text
  )
import Playwright
  ( Page
  , Playwright
  , ScreencastOverlayOptions (screencastOverlayOptionsDuration)
  , defaultGotoOptions
  , defaultScreencastOverlayOptions
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , goto
  , isHidden
  , isVisible
  , locatorCount
  , reload
  , screencastHideOverlays
  , screencastOverlayDispose
  , screencastShowOverlay
  , screencastShowOverlays
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

tests :: TestTree
tests =
  testGroup
    "screencast-overlay"
    [ testCase "should add and remove overlay" $
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

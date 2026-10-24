module Playwright.ScreencastActionsSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Exception
  ( SomeException
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
import Data.Text qualified as Text
import Playwright
  ( ActionPosition (PositionBottom, PositionBottomLeft, PositionBottomRight, PositionTop, PositionTopLeft)
  , Locator
  , Page
  , Playwright
  , ScreencastActionsHandle
  , ScreencastActionsOptions (screencastActionsOptionsDuration, screencastActionsOptionsFontSize, screencastActionsOptionsPosition)
  , click
  , defaultGotoOptions
  , defaultScreencastActionsOptions
  , evaluateOnLocatorBool
  , evaluateOnLocatorText
  , fill
  , goto
  , innerText
  , isHidden
  , isVisible
  , screencastActionsDispose
  , screencastHideActions
  , screencastShowActions
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
import UnliftIO.Async
  ( async
  )
import UnliftIO.Exception
  ( try
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

-- | Like 'eventually', but names the condition that was not met.
eventuallyNamed :: String -> Playwright Bool -> Playwright ()
eventuallyNamed label check = go (50 :: Int)
  where
    go :: Int -> Playwright ()
    go 0 = assertFailure ("condition was not met in time: " <> label)
    go n = do
      ok <- check
      unless ok $ do
        liftIO (threadDelay 100000)
        go (n - 1)

-- | Shows actions with a 5 second duration, as most upstream cases do.
showFiveSeconds :: Page -> Playwright ScreencastActionsHandle
showFiveSeconds page =
  screencastShowActions page defaultScreencastActionsOptions {screencastActionsOptionsDuration = Just 5000}

-- | Starts a click without waiting for it, as upstream's
-- @page.click(...).catch(() => {})@ does. The annotation is checked while the
-- click is in flight.
clickBg :: Locator -> Playwright ()
clickBg loc = void (async (try (click loc) :: Playwright (Either SomeException ())))

-- | Starts a fill without waiting for it, as upstream's fire-and-forget fill does.
fillBg :: Locator -> Text -> Playwright ()
fillBg loc value = void (async (try (fill loc value) :: Playwright (Either SomeException ())))

-- | Opens the button fixture and returns the URL-building prefix.
gotoButton :: Text -> Page -> Playwright ()
gotoButton prefix page =
  void (goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions)

-- | Reads a style property of the element from its live computed style.
styleOf :: Locator -> Text -> Playwright Text
styleOf loc expr = evaluateOnLocatorText loc ("el => " <> expr)

tests :: TestTree
tests =
  testGroup
    "screencast-actions"
    [ testCase "should show annotation on click" $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          void (showFiveSeconds page)
          clickBg (locate page "button")
          eventually (isVisible (locate page "x-pw-title"))
          eventually (Text.isInfixOf "click" . Text.toLower <$> innerText (locate page "x-pw-title"))
    , positionCase PositionTopLeft "top-left" [("top", "6px"), ("left", "6px")]
    , positionCase PositionTop "top" [("top", "6px"), ("left", "50%")]
    , positionCase PositionBottomLeft "bottom-left" [("bottom", "6px"), ("left", "6px")]
    , positionCase PositionBottom "bottom" [("bottom", "6px"), ("left", "50%")]
    , positionCase PositionBottomRight "bottom-right" [("bottom", "6px"), ("right", "6px")]
    , testCase "should render annotation styles" $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          void (screencastShowActions page defaultScreencastActionsOptions {screencastActionsOptionsDuration = Just 5000, screencastActionsOptionsFontSize = Just 32})
          clickBg (locate page "button")
          let title = locate page "x-pw-title"
          eventuallyNamed "title visible" (isVisible title)
          styleOf title "getComputedStyle(el).color" >>= assertEqual "title color" "rgb(255, 255, 255)"
          styleOf title "getComputedStyle(el).borderRadius" >>= assertEqual "title radius" "6px"
          styleOf title "getComputedStyle(el).padding" >>= assertEqual "title padding" "6px"
          styleOf title "el.style.top" >>= assertEqual "title top" "6px"
          styleOf title "el.style.right" >>= assertEqual "title right" "6px"
          styleOf title "el.style.fontSize" >>= assertEqual "title font size" "32px"
    , testCase "should clear annotation after duration" $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          void (screencastShowActions page defaultScreencastActionsOptions {screencastActionsOptionsDuration = Just 1000})
          clickBg (locate page "button")
          eventually (isHidden (locate page "x-pw-action-point"))
          eventually (isHidden (locate page "x-pw-title"))
    , testCase "should annotate fill action" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/textarea.html")) defaultGotoOptions
          void (showFiveSeconds page)
          fillBg (locate page "textarea") "hello"
          eventually (isVisible (locate page "x-pw-title"))
          eventually (Text.isInfixOf "fill" . Text.toLower <$> innerText (locate page "x-pw-title"))
    , testCase "should stop showing actions after dispose" $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          handle <- screencastShowActions page defaultScreencastActionsOptions {screencastActionsOptionsDuration = Just 1000}
          click (locate page "button")
          screencastActionsDispose handle
          gotoButton prefix page
          click (locate page "button")
          eventually (isHidden (locate page "x-pw-title"))
    , testCase "should stop showing actions after hideActions" $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          void (screencastShowActions page defaultScreencastActionsOptions {screencastActionsOptionsDuration = Just 1000})
          click (locate page "button")
          screencastHideActions page
          gotoButton prefix page
          click (locate page "button")
          eventually (isHidden (locate page "x-pw-title"))
    , testCase "should render an action cursor that animates to the click point" $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          void (showFiveSeconds page)
          clickBg (locate page "button")
          let cursor = locate page "x-pw-action-cursor"
          eventually (isVisible cursor)
          matches <-
            evaluateOnLocatorBool cursor "el => /\\d+px/.test(el.style.top) && /\\d+px/.test(el.style.left)"
          assertBool "cursor top and left are pixel offsets" matches
    , testCase "cursor moves between two pointer actions" $
        withAssets $ \_ page -> do
          setContent page (Text.unlines
            [ "<div style=\"position: fixed; top: 20px; left: 20px; width: 60px; height: 60px;\" id=\"a\">A</div>"
            , "<div style=\"position: fixed; bottom: 20px; right: 20px; width: 60px; height: 60px;\" id=\"b\">B</div>"
            ])
          void (showFiveSeconds page)
          let cursor = locate page "x-pw-action-cursor"
          clickBg (locate page "#a")
          eventually (isVisible cursor)
          first <- styleOf cursor "el.style.top + '|' + el.style.left"
          clickBg (locate page "#b")
          eventually ((/= first) <$> styleOf cursor "el.style.top + '|' + el.style.left")
    , testCase "should survive navigation" $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          void (showFiveSeconds page)
          clickBg (locate page "button")
          eventually (isVisible (locate page "x-pw-title"))
          gotoButton prefix page
          clickBg (locate page "button")
          eventually (isVisible (locate page "x-pw-title"))
    ]
  where
    positionCase :: ActionPosition -> Text -> [(Text, Text)] -> TestTree
    positionCase position name expected =
      testCase ("should position title at " <> Text.unpack name) $
        withAssets $ \prefix page -> do
          gotoButton prefix page
          void (screencastShowActions page defaultScreencastActionsOptions {screencastActionsOptionsDuration = Just 5000, screencastActionsOptionsPosition = Just position})
          clickBg (locate page "button")
          let title = locate page "x-pw-title"
          eventually (isVisible title)
          mapM_
            (\(prop, value) -> styleOf title ("el.style." <> prop) >>= assertEqual (Text.unpack prop) value)
            expected

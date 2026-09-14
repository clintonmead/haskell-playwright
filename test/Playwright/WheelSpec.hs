module Playwright.WheelSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  )
import Data.Text
  ( Text
  )
import Playwright
  ( Key (Key)
  , Page
  , Playwright
  , Point (Point)
  , cssPixelFromInt
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateWith
  , keyboardDown
  , mouseMove
  , mouseWheel
  , setContent
  , textArgument
  , waitForFunction
  , waitForPopup
  , wheelDeltaFromInt
  )
import Playwright.Harness
  ( withAssetPage
  , withChromiumPage
  , assertEqual
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )

tests :: TestTree
tests =
  testGroup
    "wheel"
    [ testCase "should dispatch wheel events" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 5000px; height: 5000px;\"></div>"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          listenForWheelEvents page "div"
          mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
          _ <- waitForFunction page "() => window.scrollY === 100"
          assertWheel page 0 100 50 60 False
    , testCase "should dispatch wheel events after popup was opened" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 5000px; height: 5000px;\"></div>"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          listenForWheelEvents page "div"
          withAsync (waitForPopup page) $ \pending -> do
            _ <- evaluate page "() => window.open('')"
            _ <- wait pending
            pure ()
          mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
          _ <- waitForFunction page "() => window.scrollY === 100"
          assertWheel page 0 100 50 60 False
    , testCase "should dispatch wheel event on svg element" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<body><svg class=\"scroll-box\"></svg></body><style>.scroll-box { position: absolute; top: 0px; left: 0px; background-color: brown; width: 200px; height: 200px; }</style>"
          listenForWheelEvents page "svg"
          mouseMove page (Point (cssPixelFromInt 100) (cssPixelFromInt 100))
          mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
          _ <- waitForFunction page "() => !!window.lastEvent"
          assertWheel page 0 100 100 100 False
    , testCase "should scroll when nobody is listening" $
        withAssetPage "input/scrollable.html" $ \page -> do
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
          _ <- waitForFunction page "() => window.scrollY === 100"
          pure ()
    , testCase "should set the modifiers" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 5000px; height: 5000px;\"></div>"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          listenForWheelEvents page "div"
          keyboardDown page (Key "Shift")
          mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
          _ <- waitForFunction page "() => !!window.lastEvent"
          assertWheel page 0 100 50 60 True
    , testCase "should scroll horizontally" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 5000px; height: 5000px;\"></div>"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          listenForWheelEvents page "div"
          mouseWheel page (wheelDeltaFromInt 100) (wheelDeltaFromInt 0)
          _ <- waitForFunction page "() => !!window.lastEvent"
          assertWheel page 100 0 50 60 False
          _ <- waitForFunction page "() => window.scrollX === 100"
          pure ()
    , testCase "should work when the event is canceled" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 5000px; height: 5000px;\"></div>"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          listenForWheelEvents page "div"
          _ <-
            evaluate
              page
              "() => { document.querySelector('div').addEventListener('wheel', e => e.preventDefault()); }"
          rafraf page 10
          mouseWheel page (wheelDeltaFromInt 0) (wheelDeltaFromInt 100)
          _ <- waitForFunction page "() => !!window.lastEvent"
          assertWheel page 0 100 50 60 False
          assertEqual "no scroll" (0 :: Double) =<< evaluateNumber page "() => window.scrollY"
    ]

listenForWheelEvents :: Page -> Text -> Playwright ()
listenForWheelEvents page selector = do
  _ <-
    evaluateWith
      page
      "selector => { document.querySelector(selector).addEventListener('wheel', event => { window.lastEvent = { deltaX: event.deltaX, deltaY: event.deltaY, clientX: event.clientX, clientY: event.clientY, deltaMode: event.deltaMode, ctrlKey: event.ctrlKey, shiftKey: event.shiftKey, altKey: event.altKey, metaKey: event.metaKey }; }, { passive: false }); }"
      (textArgument selector)
  pure ()

assertWheel :: Page -> Double -> Double -> Double -> Double -> Bool -> Playwright ()
assertWheel page deltaX deltaY clientX clientY shift = do
  _ <- waitForFunction page "() => !!window.lastEvent"
  assertEqual "deltaX" deltaX =<< evaluateNumber page "() => window.lastEvent.deltaX"
  assertEqual "deltaY" deltaY =<< evaluateNumber page "() => window.lastEvent.deltaY"
  assertEqual "clientX" clientX =<< evaluateNumber page "() => window.lastEvent.clientX"
  assertEqual "clientY" clientY =<< evaluateNumber page "() => window.lastEvent.clientY"
  assertEqual "deltaMode" (0 :: Double) =<< evaluateNumber page "() => window.lastEvent.deltaMode"
  assertEqual "ctrl" False =<< evaluateBool page "() => window.lastEvent.ctrlKey"
  assertEqual "shift" shift =<< evaluateBool page "() => window.lastEvent.shiftKey"
  assertEqual "alt" False =<< evaluateBool page "() => window.lastEvent.altKey"
  assertEqual "meta" False =<< evaluateBool page "() => window.lastEvent.metaKey"

rafraf :: Page -> Int -> Playwright ()
rafraf page count =
  forM_ [1 .. count] $ \(_ :: Int) -> do
    _ <-
      evaluate
        page
        "async () => { await new Promise(f => window.requestAnimationFrame(() => window.requestAnimationFrame(f))); }"
    pure ()

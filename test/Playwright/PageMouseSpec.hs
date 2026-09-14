module Playwright.PageMouseSpec
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
  , MouseButton (MouseButtonLeft, MouseButtonMiddle, MouseButtonRight)
  , Page
  , Playwright
  , Point (Point)
  , cssPixelFromDouble
  , cssPixelFromInt
  , click
  , evalOnSelectorBool
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateNumbersWith
  , evaluateText
  , evaluateTexts
  , hover
  , keyboardDown
  , keyboardUp
  , mouseClick
  , mouseClickWith
  , mouseDblclick
  , mouseDown
  , mouseDownWith
  , mouseMove
  , mouseMoveWith
  , mouseUp
  , mouseUpWith
  , selectorFromText
  , setContent
  , undefinedArgument
  )
import Playwright.Harness
  ( locate
  , withAssetPage
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

tests :: TestTree
tests =
  testGroup
    "page-mouse"
    [ testCase "should click the document" $
        withChromiumPage $ \page -> do
          _ <-
            evaluateBool
              page
              "() => { window.clickPromise = new Promise(resolve => { document.addEventListener('click', event => { resolve({ type: event.type, detail: event.detail, clientX: event.clientX, clientY: event.clientY, isTrusted: event.isTrusted, button: event.button }); }); }); return true; }"
          mouseClick page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          assertEqual "type" ("click" :: Text) =<< evaluateText page "() => window.clickPromise.then(event => event.type)"
          assertEqual "clientX" (50 :: Double) =<< evaluateNumber page "() => window.clickPromise.then(event => event.clientX)"
          assertEqual "clientY" (60 :: Double) =<< evaluateNumber page "() => window.clickPromise.then(event => event.clientY)"
          assertEqual "trusted" True =<< evaluateBool page "() => window.clickPromise.then(event => event.isTrusted)"
          assertEqual "button" (0 :: Double) =<< evaluateNumber page "() => window.clickPromise.then(event => event.button)"
    , testCase "should dblclick the div" $
        withChromiumPage $ \page -> do
          setContent page "<div style='width: 100px; height: 100px;'>Click me</div>"
          _ <-
            evaluateBool
              page
              "() => { window.dblclickPromise = new Promise(resolve => { document.querySelector('div').addEventListener('dblclick', event => { resolve({ type: event.type, detail: event.detail, clientX: event.clientX, clientY: event.clientY, isTrusted: event.isTrusted, button: event.button }); }); }); return true; }"
          mouseDblclick page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          assertEqual "type" ("dblclick" :: Text) =<< evaluateText page "() => window.dblclickPromise.then(event => event.type)"
          assertEqual "detail" (2 :: Double) =<< evaluateNumber page "() => window.dblclickPromise.then(event => event.detail)"
          assertEqual "clientX" (50 :: Double) =<< evaluateNumber page "() => window.dblclickPromise.then(event => event.clientX)"
          assertEqual "clientY" (60 :: Double) =<< evaluateNumber page "() => window.dblclickPromise.then(event => event.clientY)"
          assertEqual "trusted" True =<< evaluateBool page "() => window.dblclickPromise.then(event => event.isTrusted)"
          assertEqual "button" (0 :: Double) =<< evaluateNumber page "() => window.dblclickPromise.then(event => event.button)"
    , testCase "down and up should generate click" $
        withChromiumPage $ \page -> do
          _ <-
            evaluateBool
              page
              "() => { window.clickPromise = new Promise(resolve => { document.addEventListener('click', event => { resolve({ type: event.type, detail: event.detail, clientX: event.clientX, clientY: event.clientY, isTrusted: event.isTrusted, button: event.button }); }); }); return true; }"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          mouseDown page
          mouseUp page
          assertEqual "type" ("click" :: Text) =<< evaluateText page "() => window.clickPromise.then(event => event.type)"
          assertEqual "detail" (1 :: Double) =<< evaluateNumber page "() => window.clickPromise.then(event => event.detail)"
          assertEqual "clientX" (50 :: Double) =<< evaluateNumber page "() => window.clickPromise.then(event => event.clientX)"
          assertEqual "clientY" (60 :: Double) =<< evaluateNumber page "() => window.clickPromise.then(event => event.clientY)"
          assertEqual "trusted" True =<< evaluateBool page "() => window.clickPromise.then(event => event.isTrusted)"
          assertEqual "button" (0 :: Double) =<< evaluateNumber page "() => window.clickPromise.then(event => event.button)"
    , testCase "should pointerdown the div with a custom button" $
        withChromiumPage $ \page -> do
          setContent page "<div style='width: 100px; height: 100px;'>Click me</div>"
          _ <-
            evaluateBool
              page
              "() => { window.pointerdownPromise = new Promise(resolve => { document.querySelector('div').addEventListener('pointerdown', event => { resolve({ type: event.type, detail: event.detail, clientX: event.clientX, clientY: event.clientY, isTrusted: event.isTrusted, button: event.button, buttons: event.buttons, pointerId: event.pointerId }); }); }); return true; }"
          mouseClickWith page (Point (cssPixelFromInt 50) (cssPixelFromInt 60)) (Just MouseButtonMiddle) Nothing
          assertEqual "type" ("pointerdown" :: Text) =<< evaluateText page "() => window.pointerdownPromise.then(event => event.type)"
          assertEqual "detail" (0 :: Double) =<< evaluateNumber page "() => window.pointerdownPromise.then(event => event.detail)"
          assertEqual "clientX" (50 :: Double) =<< evaluateNumber page "() => window.pointerdownPromise.then(event => event.clientX)"
          assertEqual "clientY" (60 :: Double) =<< evaluateNumber page "() => window.pointerdownPromise.then(event => event.clientY)"
          assertEqual "trusted" True =<< evaluateBool page "() => window.pointerdownPromise.then(event => event.isTrusted)"
          assertEqual "button" (1 :: Double) =<< evaluateNumber page "() => window.pointerdownPromise.then(event => event.button)"
          assertEqual "buttons" (4 :: Double) =<< evaluateNumber page "() => window.pointerdownPromise.then(event => event.buttons)"
          assertEqual "pointerId" (1 :: Double) =<< evaluateNumber page "() => window.pointerdownPromise.then(event => event.pointerId)"
    , testCase "should report correct buttons property" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.__EVENTS = []; const handler = event => { window.__EVENTS.push({ type: event.type, button: event.button, buttons: event.buttons }); }; window.addEventListener('mousedown', handler, false); window.addEventListener('mouseup', handler, false); }"
          mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          mouseDownWith page (Just MouseButtonMiddle)
          mouseDownWith page (Just MouseButtonLeft)
          mouseUpWith page (Just MouseButtonMiddle)
          mouseUpWith page (Just MouseButtonLeft)
          types <- evaluateTexts page "() => window.__EVENTS.map(e => e.type)"
          buttons <- evaluateNumbersWith page "() => window.__EVENTS.map(e => e.button)" undefinedArgument
          buttonMasks <- evaluateNumbersWith page "() => window.__EVENTS.map(e => e.buttons)" undefinedArgument
          assertEqual "types" ["mousedown", "mousedown", "mouseup", "mouseup" :: Text] types
          assertEqual "button" [1, 0, 1, 0 :: Double] buttons
          assertEqual "buttons" [4, 5, 1, 0 :: Double] buttonMasks
    , testCase "should trigger hover state" $
        withAssetPage "input/scrollable.html" $ \page -> do
          hover (locate page "#button-6")
          assertEqual "6" "button-6" =<< evaluateText page "() => document.querySelector('button:hover').id"
          hover (locate page "#button-2")
          assertEqual "2" "button-2" =<< evaluateText page "() => document.querySelector('button:hover').id"
          hover (locate page "#button-91")
          assertEqual "91" "button-91" =<< evaluateText page "() => document.querySelector('button:hover').id"
    , testCase "should trigger hover state on disabled button" $
        withAssetPage "input/scrollable.html" $ \page -> do
          _ <- evalOnSelectorBool page (selectorFromText "#button-6") "button => { button.disabled = true; return true; }"
          hover (locate page "#button-6")
          assertEqual "hovered" "button-6" =<< evaluateText page "() => document.querySelector('button:hover').id"
    , testCase "should trigger hover state with removed window.Node" $
        withAssetPage "input/scrollable.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          hover (locate page "#button-6")
          assertEqual "hovered" "button-6" =<< evaluateText page "() => document.querySelector('button:hover').id"
    , testCase "should set modifier keys on click" $
        withAssetPage "input/scrollable.html" $ \page -> do
          _ <-
            evaluate
              page
              "() => { document.querySelector('#button-3').addEventListener('mousedown', e => window.lastEvent = e, true); }"
          forM_ modifiers $ \(key, field) -> do
            keyboardDown page (Key key)
            clickButton3 page
            assertEqual (show field) True =<< evaluateBool page ("() => window.lastEvent." <> field)
            keyboardUp page (Key key)
          clickButton3 page
          forM_ modifiers $ \(_, field) ->
            assertEqual (show field) False =<< evaluateBool page ("() => window.lastEvent." <> field)
    , testCase "should tween mouse movement" $
        withChromiumPage $ \page -> do
          mouseMove page (Point (cssPixelFromInt 100) (cssPixelFromInt 100))
          _ <-
            evaluate
              page
              "() => { window.result = []; document.addEventListener('mousemove', event => { window.result.push([event.clientX, event.clientY]); }); }"
          mouseMoveWith page (Point (cssPixelFromInt 200) (cssPixelFromInt 300)) (Just 5)
          xs <- evaluateNumbersWith page "() => window.result.map(p => p[0])" undefinedArgument
          ys <- evaluateNumbersWith page "() => window.result.map(p => p[1])" undefinedArgument
          assertEqual "x" [120, 140, 160, 180, 200 :: Double] xs
          assertEqual "y" [140, 180, 220, 260, 300 :: Double] ys
    , testCase "should always round down" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { document.addEventListener('mousedown', event => { window.result = [event.clientX, event.clientY]; }); }"
          mouseClick page (Point (cssPixelFromDouble 50.1) (cssPixelFromDouble 50.9))
          xs <- evaluateNumbersWith page "() => window.result" undefinedArgument
          assertEqual "rounded" [50, 50 :: Double] xs
    , testCase "should not crash on mouse drag with any button" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.addEventListener('contextmenu', e => e.preventDefault(), false); }"
          forM_ [MouseButtonLeft, MouseButtonMiddle, MouseButtonRight] $ \button -> do
            mouseMove page (Point (cssPixelFromInt 50) (cssPixelFromInt 50))
            mouseDownWith page (Just button)
            mouseMove page (Point (cssPixelFromInt 100) (cssPixelFromInt 100))
    ]

modifiers :: [(Text, Text)]
modifiers =
  [ ("Shift", "shiftKey")
  , ("Control", "ctrlKey")
  , ("Alt", "altKey")
  , ("Meta", "metaKey")
  ]

clickButton3 :: Page -> Playwright ()
clickButton3 page =
  click (locate page "#button-3")

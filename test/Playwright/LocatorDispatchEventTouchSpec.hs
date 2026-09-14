module Playwright.LocatorDispatchEventTouchSpec
  ( tests
  ) where

import Playwright
  ( DomEventType (DomEventType)
  , NewContextOptions (newContextHasTouch)
  , arrayValue
  , defaultNewContextOptions
  , dispatchEventWith
  , evaluateOnLocator
  , evaluateTexts
  , getByTestId
  , numberValue
  , objectArgument
  , objectValue
  , setContent
  )
import Playwright.Harness
  ( withChromiumContextWith
  , assertEqual
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedValue
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
    "locator-dispatchevent-touch"
    [ testCase "should support touch points in touch event arguments" $
        withChromiumContextWith
          defaultNewContextOptions {newContextHasTouch = Just True}
          $ \_ page -> do
            setContent
              page
              "<div data-testid='outer' style=\"position: absolute; width: 120px; height: 120px; background-color: red;\"><div data-testid='inner' style=\"position: absolute; width: 100px; height: 100px; top: 10px; left: 10px; background-color: green; z-index: 3;\">inner</div></div>"
            _ <-
              evaluateOnLocator
                (getByTestId page "outer")
                "el => { const events = []; window.events = events; el.addEventListener('touchstart', e => events.push('touchstart: ' + [...e.touches].map(t => `${t.constructor.name}(id: ${t.identifier}, clientX: ${t.clientX}, clientY: ${t.clientY})`))); el.addEventListener('touchmove', e => events.push('touchmove: ' + [...e.touches].map(t => `${t.constructor.name}(id: ${t.identifier}, clientX: ${t.clientX}, clientY: ${t.clientY})`))); el.addEventListener('touchend', e => events.push('touchend: ' + [...e.touches].map(t => `${t.constructor.name}(id: ${t.identifier}, clientX: ${t.clientX}, clientY: ${t.clientY})`))); }"
            let
              inner = getByTestId page "inner"
              touches = [touchPoint 0 61 60, touchPoint 1 59 60]
            dispatchEventWith
              inner
              (DomEventType "touchstart")
              (touchInit touches touches touches)
            dispatchEventWith
              inner
              (DomEventType "touchmove")
              (touchInit touches touches touches)
            dispatchEventWith
              inner
              (DomEventType "touchend")
              (touchInit [] touches [])
            assertEqual
              "events"
              [ "touchstart: Touch(id: 0, clientX: 61, clientY: 60),Touch(id: 1, clientX: 59, clientY: 60)"
              , "touchmove: Touch(id: 0, clientX: 61, clientY: 60),Touch(id: 1, clientX: 59, clientY: 60)"
              , "touchend: "
              ]
              =<< evaluateTexts page "() => window.events"
    ]

touchPoint :: Double -> Double -> Double -> SerializedValue
touchPoint identifier clientX clientY =
  objectValue
    [ ("identifier", numberValue identifier)
    , ("clientX", numberValue clientX)
    , ("clientY", numberValue clientY)
    ]

touchInit :: [SerializedValue] -> [SerializedValue] -> [SerializedValue] -> SerializedArgument
touchInit touches changed target =
  objectArgument
    [ ("touches", arrayValue touches)
    , ("changedTouches", arrayValue changed)
    , ("targetTouches", arrayValue target)
    ]

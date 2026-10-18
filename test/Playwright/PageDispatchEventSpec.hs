module Playwright.PageDispatchEventSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( Text
  , replace
  )
import Playwright
  ( DomEventType (DomEventType)
  , Frame
  , Page
  , Playwright
  , boolValue
  , childLocator
  , defaultGotoOptions
  , dispatchEvent
  , dispatchEventHandle
  , dispatchEventHandleWith
  , dispatchEventWith
  , evaluate
  , evaluateBool
  , evaluateHandle
  , evaluateHandleOnFrame
  , evaluateHandleOnLocator
  , evaluateNumber
  , evaluateOnHandleBool
  , evaluateOnHandleNumber
  , evaluateOnLocatorBool
  , evaluateText
  , frameLocator
  , frames
  , goto
  , jsHandleObjectArgument
  , numberValue
  , objectArgument
  , objectNumberArgument
  , objectValue
  , querySelector
  , registerSelector
  , selectorFromText
  , setContent
  , urlFromText
  , waitForTimeout
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , locate
  , requireHandle
  , withAssetPage
  , withAssets
  , withChromiumPage
  , assertEqual
  , assertFailure
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
    "page-dispatchevent"
    [ testCase "should dispatch click event" $
        withAssetPage "input/button.html" $ \page -> do
          dispatchEvent (locate page "button") (DomEventType "click")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should dispatch click event properties" $
        withAssetPage "input/button.html" $ \page -> do
          dispatchEvent (locate page "button") (DomEventType "click")
          assertEqual "bubbles" True =<< evaluateBool page "() => window.bubbles"
          assertEqual "cancelable" True =<< evaluateBool page "() => window.cancelable"
          assertEqual "composed" True =<< evaluateBool page "() => window.composed"
    , testCase "should dispatch click svg" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<svg height=\"100\" width=\"100\"><circle onclick=\"window.__CLICKED=42\" cx=\"50\" cy=\"50\" r=\"40\" stroke=\"black\" stroke-width=\"3\" fill=\"red\" /></svg>"
          dispatchEvent (locate page "circle") (DomEventType "click")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.__CLICKED"
    , testCase "should dispatch click on a span with an inline element inside" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>span::before { content: 'q'; }</style><span onclick='window.CLICKED=42'></span>"
          dispatchEvent (locate page "span") (DomEventType "click")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.CLICKED"
    , testCase "should dispatch click after navigation" $
        withAssets $ \prefix page -> do
          clickButton page (prefix <> "/input/button.html")
          clickButton page (prefix <> "/input/button.html")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should dispatch click after a cross origin navigation" $
        withAssets $ \prefix page -> do
          clickButton page (prefix <> "/input/button.html")
          clickButton page (replace "127.0.0.1" "localhost" prefix <> "/input/button.html")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should not fail when element is blocked on hover" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>container { display: block; position: relative; width: 200px; height: 50px; } div, button { position: absolute; left: 0; top: 0; bottom: 0; right: 0; } div { pointer-events: none; } container:hover div { pointer-events: auto; background: red; }</style><container><button onclick=\"window.clicked=true\">Click me</button><div></div></container>"
          dispatchEvent (locate page "button") (DomEventType "click")
          assertEqual "clicked" True =<< evaluateBool page "() => window.clicked"
    , testCase "should dispatch click when node is added in shadow dom" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          withAsync (dispatchEvent (locate page "span") (DomEventType "click")) $ \pending -> do
            _ <-
              evaluate
                page
                "() => { const div = document.createElement('div'); div.attachShadow({ mode: 'open' }); document.body.appendChild(div); }"
            waitForTimeout page =<< requireTimeout 100
            _ <-
              evaluate
                page
                "() => { const span = document.createElement('span'); span.textContent = 'Hello from shadow'; span.addEventListener('click', () => window.clicked = true); document.querySelector('div').shadowRoot.appendChild(span); }"
            wait pending
          assertEqual "clicked" True =<< evaluateBool page "() => window.clicked"
    , testCase "should be atomic" $
        withChromiumPage $ \page -> do
          registerSelector "dispatchEvent" dispatchEventEngine
          _ <-
            goto
              page
              (urlFromText "data:text/html,<div onclick=\"window._clicked=true\">Hello</div>")
              defaultGotoOptions
          dispatchEvent (locate page "dispatchEvent=div") (DomEventType "click")
          assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"
    , testCase "should dispatch drag drop events" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          dataTransfer <- evaluateHandle page "() => new DataTransfer()"
          dispatchEventWith
            (locate page "#source")
            (DomEventType "dragstart")
            (jsHandleObjectArgument "dataTransfer" dataTransfer)
          dispatchEventWith
            (locate page "#target")
            (DomEventType "drop")
            (jsHandleObjectArgument "dataTransfer" dataTransfer)
          assertDropped page
    , testCase "should dispatch drag drop events via ElementHandles" $
        withAssetPage "drag-n-drop.html" $ \page -> do
          dataTransfer <- evaluateHandle page "() => new DataTransfer()"
          source <- requireHandle =<< querySelector page (selectorFromText "#source")
          dispatchEventHandleWith
            source
            (DomEventType "dragstart")
            (jsHandleObjectArgument "dataTransfer" dataTransfer)
          target <- requireHandle =<< querySelector page (selectorFromText "#target")
          dispatchEventHandleWith
            target
            (DomEventType "drop")
            (jsHandleObjectArgument "dataTransfer" dataTransfer)
          assertDropped page
    , testCase "should dispatch click event via ElementHandles" $
        withAssetPage "input/button.html" $ \page -> do
          button <- requireHandle =<< querySelector page (selectorFromText "button")
          dispatchEventHandle button (DomEventType "click")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should dispatch wheel event" $
        withAssetPage "input/scrollable.html" $ \page -> do
          eventsHandle <-
            evaluateHandleOnLocator
              (locate page "body")
              "e => { const events = []; e.addEventListener('wheel', event => events.push(event)); return events; }"
          dispatchEventWith
            (locate page "body")
            (DomEventType "wheel")
            (objectNumberArgument [("deltaX", 100), ("deltaY", 200)])
          assertEqual "count" (1 :: Double) =<< evaluateOnHandleNumber eventsHandle "e => e.length"
          assertEqual "wheel" True =<< evaluateOnHandleBool eventsHandle "e => e[0] instanceof WheelEvent"
          assertEqual "deltaX" (100 :: Double) =<< evaluateOnHandleNumber eventsHandle "e => e[0].deltaX"
          assertEqual "deltaY" (200 :: Double) =<< evaluateOnHandleNumber eventsHandle "e => e[0].deltaY"
    , testCase "should dispatch device orientation event" $
        withAssetPage "device-orientation.html" $ \page -> do
          dispatchEventWith
            (locate page "html")
            (DomEventType "deviceorientation")
            (objectNumberArgument [("alpha", 10), ("beta", 20), ("gamma", 30)])
          assertEqual "result" "Oriented" =<< evaluateText page "() => window.result"
          assertEqual "alpha" (10 :: Double) =<< evaluateNumber page "() => window.alpha"
          assertEqual "beta" (20 :: Double) =<< evaluateNumber page "() => window.beta"
          assertEqual "gamma" (30 :: Double) =<< evaluateNumber page "() => window.gamma"
          assertEqual "absolute" False =<< evaluateBool page "() => window.absolute"
    , testCase "should dispatch absolute device orientation event" $
        withAssetPage "device-orientation.html" $ \page -> do
          dispatchEventWith
            (locate page "html")
            (DomEventType "deviceorientationabsolute")
            ( objectArgument
                [ ("alpha", numberValue 10)
                , ("beta", numberValue 20)
                , ("gamma", numberValue 30)
                , ("absolute", boolValue True)
                ]
            )
          assertEqual "result" "Oriented" =<< evaluateText page "() => window.result"
          assertEqual "alpha" (10 :: Double) =<< evaluateNumber page "() => window.alpha"
          assertEqual "beta" (20 :: Double) =<< evaluateNumber page "() => window.beta"
          assertEqual "gamma" (30 :: Double) =<< evaluateNumber page "() => window.gamma"
          assertEqual "absolute" True =<< evaluateBool page "() => window.absolute"
    , testCase "should dispatch device motion event" $
        withAssetPage "device-motion.html" $ \page -> do
          dispatchEventWith
            (locate page "html")
            (DomEventType "devicemotion")
            ( objectArgument
                [ ("acceleration", vector3 10 20 30)
                , ("accelerationIncludingGravity", vector3 15 25 35)
                , ("rotationRate", rotationRate 5 10 15)
                , ("interval", numberValue 16)
                ]
            )
          assertEqual "result" "Moved" =<< evaluateText page "() => window.result"
          assertEqual "ax" (10 :: Double) =<< evaluateNumber page "() => window.acceleration.x"
          assertEqual "ay" (20 :: Double) =<< evaluateNumber page "() => window.acceleration.y"
          assertEqual "az" (30 :: Double) =<< evaluateNumber page "() => window.acceleration.z"
          assertEqual "gx" (15 :: Double) =<< evaluateNumber page "() => window.accelerationIncludingGravity.x"
          assertEqual "gy" (25 :: Double) =<< evaluateNumber page "() => window.accelerationIncludingGravity.y"
          assertEqual "gz" (35 :: Double) =<< evaluateNumber page "() => window.accelerationIncludingGravity.z"
          assertEqual "ra" (5 :: Double) =<< evaluateNumber page "() => window.rotationRate.alpha"
          assertEqual "rb" (10 :: Double) =<< evaluateNumber page "() => window.rotationRate.beta"
          assertEqual "rg" (15 :: Double) =<< evaluateNumber page "() => window.rotationRate.gamma"
          assertEqual "interval" (16 :: Double) =<< evaluateNumber page "() => window.interval"
    , testCase "should throw if argument is from different frame" $
        withAssetPage "frames/one-frame.html" $ \page -> do
          child <- iframeFrame page
          dataTransfer <- evaluateHandleOnFrame child "() => new DataTransfer()"
          dispatchEventWith
            (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "div"))
            (DomEventType "drop")
            (jsHandleObjectArgument "dataTransfer" dataTransfer)
          mainHandle <- evaluateHandle page "() => new DataTransfer()"
          assertProtocolContains
            "JSHandles can be evaluated only in the context they were created"
            ( dispatchEventWith
                (childLocator (frameLocator page (selectorFromText "iframe")) (selectorFromText "div"))
                (DomEventType "drop")
                (jsHandleObjectArgument "dataTransfer" mainHandle)
            )
    ]

clickButton :: Page -> Text -> Playwright ()
clickButton page path = do
  _ <- goto page (urlFromText path) defaultGotoOptions
  dispatchEvent (locate page "button") (DomEventType "click")

assertDropped :: Page -> Playwright ()
assertDropped page =
  assertEqual "dropped" True
    =<< evaluateOnLocatorBool
      (locate page "#target")
      "target => target.contains(document.querySelector('#source'))"

iframeFrame :: Page -> Playwright Frame
iframeFrame page = do
  allFrames <- frames page
  case drop 1 allFrames of
    frame : _ ->
      pure frame
    [] ->
      assertFailure "expected iframe"

vector3 :: Double -> Double -> Double -> SerializedValue
vector3 x y z =
  objectValue
    [ ("x", numberValue x)
    , ("y", numberValue y)
    , ("z", numberValue z)
    ]

rotationRate :: Double -> Double -> Double -> SerializedValue
rotationRate alpha beta gamma =
  objectValue
    [ ("alpha", numberValue alpha)
    , ("beta", numberValue beta)
    , ("gamma", numberValue gamma)
    ]

dispatchEventEngine :: Text
dispatchEventEngine =
  "(() => ({\n\
  \  query(root, selector) {\n\
  \    const result = root.querySelector(selector);\n\
  \    if (result)\n\
  \      void Promise.resolve().then(() => result.onclick = '');\n\
  \    return result;\n\
  \  },\n\
  \  queryAll(root, selector) {\n\
  \    const result = Array.from(root.querySelectorAll(selector));\n\
  \    for (const e of result)\n\
  \      void Promise.resolve().then(() => e.onclick = null);\n\
  \    return result;\n\
  \  }\n\
  \}))()"


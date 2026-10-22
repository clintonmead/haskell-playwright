module Playwright.TapSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import UnliftIO.Async
  ( wait
  , withAsync
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import UnliftIO.Timeout
  ( timeout
  )
import Data.Text
  ( Text
  )
import Playwright
  ( ElementHandle
  , JsHandle
  , NewContextOptions (newContextHasTouch)
  , Page
  , Point (Point)
  , UrlMatch (UrlMatchGlob)
  , asElement
  , cssPixelFromInt
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluate
  , evaluateBool
  , evaluateText
  , defaultTapOptions
  , goto
  , KeyboardModifier (KeyboardModifierAlt)
  , route
  , TapOptions (tapOptionsModifiers, tapOptionsTrial)
  , tapWith
  , touchscreenTap
  , urlFromText
  , evaluateHandle
  , evaluateHandleOnElement
  , evaluateOnElement
  , jsonValueTexts
  , locator
  , querySelector
  , selectorFromText
  , setContent
  , tap
  , tapHandle
  , textContentHandle
  , Playwright
  )
import Playwright.Harness
  ( locate
  , requireHandle
  , withAssetsWith
  , withChromiumContextWith
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
    "tap"
    [ testCase "should send all of the correct events" $
        withTouchPage $ \page -> do
          setContent
            page
            "<div id=\"a\" style=\"background: lightblue; width: 50px; height: 50px\">a</div><div id=\"b\" style=\"background: pink; width: 50px; height: 50px\">b</div>"
          tap (locate page "#a")
          target <- requireHandle =<< querySelector page (selectorFromText "#b")
          eventsHandle <- trackEvents target
          tap (locate page "#b")
          assertEqual "events" chromiumTapEvents =<< jsonValueTexts eventsHandle
    , testCase "should not send mouse events touchstart is canceled" $
        withTouchPage $ \page -> do
          setContent page "<div style=\"width: 50px; height: 50px; background: red\">"
          _ <-
            evaluate
              page
              "() => { document.addEventListener('touchstart', t => t.preventDefault(), { passive: false }); }"
          target <- requireHandle =<< querySelector page (selectorFromText "div")
          eventsHandle <- trackEvents target
          tap (locate page "div")
          assertEqual "canceled touchstart" canceledTapEvents =<< jsonValueTexts eventsHandle
    , testCase "should not send mouse events when touchend is canceled" $
        withTouchPage $ \page -> do
          setContent page "<div style=\"width: 50px; height: 50px; background: red\">"
          _ <-
            evaluate
              page
              "() => { document.addEventListener('touchend', t => t.preventDefault(), { passive: false }); }"
          target <- requireHandle =<< querySelector page (selectorFromText "div")
          eventsHandle <- trackEvents target
          tap (locate page "div")
          assertEqual "canceled touchend" canceledTapEvents =<< jsonValueTexts eventsHandle
    , testCase "should not wait for a navigation caused by a tap" $
        withAssetsWith defaultNewContextOptions {newContextHasTouch = Just True} $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page "<a href=\"/intercept-this.html\">link</a>;"
          routed <- newEmptyMVar
          route page (UrlMatchGlob "**/intercept-this.html") (\_ -> void (tryPutMVar routed ()))
          tap (locate page "a")
          hit <- timeout 10000000 (takeMVar routed)
          assertEqual "navigation request reached the route" (Just ()) hit
    , testCase "should send well formed touch points" $
        withTouchPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { window.events = {}; for (const type of ['touchstart', 'touchend']) document.addEventListener(type, event => { window.events[type] = [...event.touches].map(t => ({ identifier: t.identifier, clientX: t.clientX, clientY: t.clientY, pageX: t.pageX, pageY: t.pageY, radiusX: t.radiusX, radiusY: t.radiusY, rotationAngle: t.rotationAngle, force: t.force })); }, false); }"
          touchscreenTap page (Point (cssPixelFromInt 40) (cssPixelFromInt 60))
          assertEqual
            "touch points"
            "{\"touchstart\":[{\"identifier\":0,\"clientX\":40,\"clientY\":60,\"pageX\":40,\"pageY\":60,\"radiusX\":1,\"radiusY\":1,\"rotationAngle\":0,\"force\":1}],\"touchend\":[]}"
            =<< evaluateText page "() => JSON.stringify(window.events)"
    , testCase "should wait until an element is visible to tap it" $
        withTouchPage $ \page -> do
          jsHandle <-
            evaluateHandle
              page
              "() => { const button = document.createElement('button'); button.textContent = 'not clicked'; document.body.appendChild(button); button.style.display = 'none'; return button; }"
          handle <- requireHandle =<< asElement jsHandle
          withAsync (tapHandle handle) $ \pending -> do
            _ <- evaluateOnElement handle "div => { div.onclick = () => div.textContent = 'clicked' }"
            _ <- evaluateOnElement handle "div => { div.style.display = 'block' }"
            wait pending
          assertEqual "clicked" (Just "clicked") =<< textContentHandle handle
    , testCase "locators should send all of the correct events" $
        withTouchPage $ \page -> do
          setContent
            page
            "<div id=\"a\" style=\"background: lightblue; width: 50px; height: 50px\">a</div><div id=\"b\" style=\"background: pink; width: 50px; height: 50px\">b</div>"
          tap (locator page (selectorFromText "#a"))
          tap (locator page (selectorFromText "#b"))
    , testCase "should work with modifiers" $
        withChromiumContextWith defaultNewContextOptions {newContextHasTouch = Just True} $ \_ page -> do
          setContent page "hello world"
          _ <-
            evaluate
              page
              "() => { window.altKey = null; document.addEventListener('touchstart', event => { window.altKey = event.altKey; }, { passive: false }); }"
          tapWith (locate page "body") defaultTapOptions {tapOptionsModifiers = Just [KeyboardModifierAlt]}
          assertEqual "alt key" True =<< evaluateBool page "() => window.altKey === true"
    , testCase "trial run should not tap" $
        withChromiumContextWith defaultNewContextOptions {newContextHasTouch = Just True} $ \_ page -> do
          setContent
            page
            "<div id=\"a\" style=\"background: lightblue; width: 50px; height: 50px\">a</div><div id=\"b\" style=\"background: pink; width: 50px; height: 50px\">b</div>"
          tapHandle =<< requireHandle =<< querySelector page (selectorFromText "#a")
          _ <-
            evaluate
              page
              "() => { window.events = []; const b = document.querySelector('#b'); for (const t of ['pointerover', 'pointerenter', 'pointerout', 'pointerleave']) b.addEventListener(t, () => window.events.push(t)); }"
          tapWith (locate page "#b") defaultTapOptions {tapOptionsTrial = Just True}
          assertEqual "events" "[\"pointerover\",\"pointerenter\",\"pointerout\",\"pointerleave\"]"
            =<< evaluateText page "() => JSON.stringify(window.events)"
    ]

withTouchPage :: (Page -> Playwright a) -> IO a
withTouchPage action =
  withChromiumContextWith
    defaultNewContextOptions {newContextHasTouch = Just True}
    (\_ page -> action page)

trackEvents :: ElementHandle -> Playwright JsHandle
trackEvents target =
  evaluateHandleOnElement
    target
    "target => { const events = []; for (const event of ['mousedown', 'mouseenter', 'mouseleave', 'mousemove', 'mouseout', 'mouseover', 'mouseup', 'click', 'pointercancel', 'pointerdown', 'pointerenter', 'pointerleave', 'pointermove', 'pointerout', 'pointerover', 'pointerup', 'touchstart', 'touchend', 'touchmove', 'touchcancel']) target.addEventListener(event, () => events.push(event), false); return events; }"

-- | Events a tap delivers to its target when the touch sequence is cancelled,
-- so no mouse events follow.
canceledTapEvents :: [Text]
canceledTapEvents =
  [ "pointerover"
  , "pointerenter"
  , "pointerdown"
  , "touchstart"
  , "pointerup"
  , "pointerout"
  , "pointerleave"
  , "touchend"
  ]

chromiumTapEvents :: [Text]
chromiumTapEvents =
  [ "pointerover"
  , "pointerenter"
  , "pointerdown"
  , "touchstart"
  , "pointerup"
  , "pointerout"
  , "pointerleave"
  , "touchend"
  , "mouseover"
  , "mouseenter"
  , "mousemove"
  , "mousedown"
  , "mouseup"
  , "click"
  ]

module Playwright.TapSpec
  ( tests
  ) where

import UnliftIO.Async
  ( wait
  , withAsync
  )
import Data.Text
  ( Text
  )
import Playwright
  ( ElementHandle
  , JsHandle
  , NewContextOptions (newContextHasTouch)
  , Page
  , asElement
  , defaultNewContextOptions
  , evaluate
  , evaluateBool
  , evaluateText
  , defaultTapOptions
  , KeyboardModifier (KeyboardModifierAlt)
  , TapOptions (tapOptionsModifiers, tapOptionsTrial)
  , tapWith
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
          assertEqual
            "canceled touchstart"
            [ "pointerover"
            , "pointerenter"
            , "pointerdown"
            , "touchstart"
            , "pointerup"
            , "pointerout"
            , "pointerleave"
            , "touchend"
            ]
            =<< jsonValueTexts eventsHandle
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

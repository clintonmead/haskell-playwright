module Playwright.PageClickTimeoutSpec
  ( tests
  ) where

import Playwright
  ( ClickOptions (clickOptionsForce, clickOptionsTimeout)
  , Page
  , Playwright
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize, viewportHeight, viewportWidth)
  , ViewportWidth (ViewportWidth)
  , clickWith
  , defaultClickOptions
  , evaluate
  , evaluateBool
  , evaluateNumber
  , milliseconds
  , setContent
  , setViewportSize
  )
import Playwright.Harness
  ( assertBool
  , assertProtocolContainsAll
  , locate
  , rafraf
  , withChromiumPage
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
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
    "page-click-timeout"
    [ testCase "should timeout waiting for button to be enabled" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button onclick=\"javascript:window.__CLICKED=true;\" disabled><span>Click target</span></button>"
          assertProtocolContainsAll
            [ "Timeout 3000ms exceeded"
            , "element is not enabled"
            , "retrying click action"
            ]
            $ clickWith
              (locate page "text=Click target")
              defaultClickOptions {clickOptionsTimeout = milliseconds 3000}
          assertBool "not clicked" =<< evaluateBool page "() => window.__CLICKED === undefined"
    , testCase "should timeout waiting for display:none to be gone" $
        withButton $ \page -> do
          _ <- evaluate page "() => { document.querySelector('button').style.display = 'none'; }"
          assertWaitingForVisibleClick page 5000
    , testCase "should timeout waiting for visibility:hidden to be gone" $
        withButton $ \page -> do
          _ <- evaluate page "() => { document.querySelector('button').style.visibility = 'hidden'; }"
          assertWaitingForVisibleClick page 5000
    , testCase "should timeout waiting for hit target" $
        withButton $ \page -> do
          addBlocker page "400px" "20px"
          assertProtocolContainsAll
            [ "Timeout 5000ms exceeded"
            , "<div id=\"blocker\"></div> intercepts pointer events"
            , "retrying click action"
            , "waiting 500ms"
            ]
            $ clickWith
              (locate page "button")
              defaultClickOptions {clickOptionsTimeout = milliseconds 5000}
    , testCase "should still click when force but hit target is obscured" $
        withButton $ \page -> do
          addBlocker page "400px" "200px"
          clickWith
            (locate page "button")
            defaultClickOptions {clickOptionsForce = Just True}
    , testCase "should report wrong hit target subtree" $
        withButton $ \page -> do
          _ <-
            evaluate
              page
              ( "() => {"
                  <> " document.body.style.position = 'relative';"
                  <> " const blocker = document.createElement('div');"
                  <> " blocker.id = 'blocker';"
                  <> " blocker.style.position = 'absolute';"
                  <> " blocker.style.width = '400px';"
                  <> " blocker.style.height = '20px';"
                  <> " blocker.style.left = '0';"
                  <> " blocker.style.top = '0';"
                  <> " document.body.appendChild(blocker);"
                  <> " const inner = document.createElement('div');"
                  <> " inner.id = 'inner';"
                  <> " inner.style.position = 'absolute';"
                  <> " inner.style.left = '0';"
                  <> " inner.style.top = '0';"
                  <> " inner.style.right = '0';"
                  <> " inner.style.bottom = '0';"
                  <> " blocker.appendChild(inner);"
                  <> " }"
              )
          assertProtocolContainsAll
            [ "Timeout 5000ms exceeded"
            , "<div id=\"inner\"></div> from <div id=\"blocker\">…</div> subtree intercepts pointer events"
            , "retrying click action"
            ]
            $ clickWith
              (locate page "button")
              defaultClickOptions {clickOptionsTimeout = milliseconds 5000}
    , testCase "should timeout waiting for stable position" $
        withButton $ \page -> do
          _ <-
            evaluate
              page
              ( "() => {"
                  <> " const button = document.querySelector('button');"
                  <> " button.style.transition = 'margin 5s linear 0s';"
                  <> " button.style.marginLeft = '200px';"
                  <> " }"
              )
          -- Lets the animation start before the click measures the position.
          rafraf page 2
          assertProtocolContainsAll
            [ "Timeout 3000ms exceeded"
            , "waiting for element to be visible, enabled and stable"
            , "element is not stable"
            , "retrying click action"
            ]
            $ clickWith
              (locate page "button")
              defaultClickOptions {clickOptionsTimeout = milliseconds 3000}
    , testCase "should fail to click the button behind a large header after scrolling around" $
        withChromiumPage $ \page -> do
          setViewportSize
            page
            ViewportSize
              { viewportWidth = ViewportWidth 500
              , viewportHeight = ViewportHeight 240
              }
          setContent page largeHeaderMarkup
          _ <- evaluate page "() => document.querySelector('#li14').scrollIntoView()"
          assertProtocolContainsAll
            ["<div class=\"fixed\"></div> intercepts pointer events"]
            $ clickWith
              (locate page "#target")
              defaultClickOptions {clickOptionsTimeout = milliseconds 3500}
          assertBool "not clicked" =<< evaluateBool page "() => window.__clicked === undefined"
          distinct <- evaluateNumber page "() => new Set(window.scrollTops).size"
          assertBool "scroll positions" (distinct > 2)
    -- Skipped: "should avoid side effects after timeout", "should fail when element jumps
    -- during hit testing" and "should click for the second time after first timeout". Upstream
    -- passes the test-only `__testHookBeforePointerAction` or `__testHookBeforeHitTarget`
    -- option, a JavaScript function that the driver runs inside the action between
    -- checks. A function cannot cross the JSON protocol to the separate driver process, and
    -- the Haskell client has no equivalent hook, so the mid-action timing cannot be reproduced.
    -- "should avoid side effects after timeout"
    -- "should fail when element jumps during hit testing"
    -- "should click for the second time after first timeout"
    ]

-- | Runs the test against a page holding one plain button.
withButton :: (Page -> Playwright a) -> IO a
withButton action =
  withChromiumPage $ \page -> do
    setContent page "<button>Click target</button>"
    action page

-- | Adds an absolutely positioned div over the page's top left corner, which
-- then intercepts pointer events meant for the button.
addBlocker :: Page -> Text -> Text -> Playwright ()
addBlocker page width height = do
  _ <-
    evaluate
      page
      ( "() => {"
          <> " document.body.style.position = 'relative';"
          <> " const blocker = document.createElement('div');"
          <> " blocker.id = 'blocker';"
          <> " blocker.style.position = 'absolute';"
          <> " blocker.style.width = '"
          <> width
          <> "';"
          <> " blocker.style.height = '"
          <> height
          <> "';"
          <> " blocker.style.left = '0';"
          <> " blocker.style.top = '0';"
          <> " document.body.appendChild(blocker);"
          <> " }"
      )
  pure ()

-- | Clicks a button hidden by the given style, and expects the click to time out
-- with the visibility diagnostics.
assertWaitingForVisibleClick :: Page -> Int -> Playwright ()
assertWaitingForVisibleClick page timeout =
  assertProtocolContainsAll
    [ "Timeout " <> Text.pack (show timeout) <> "ms exceeded"
    , "waiting for element to be visible, enabled and stable"
    , "element is not visible"
    , "retrying click action"
    ]
    $ clickWith
      (locate page "button")
      defaultClickOptions {clickOptionsTimeout = milliseconds (fromIntegral timeout)}

largeHeaderMarkup :: Text
largeHeaderMarkup =
  Text.unlines
    [ "<style>"
    , "* { padding: 0; margin: 0; }"
    , "li { height: 80px; border: 1px solid black; }"
    , "ol { padding-top: 160px; }"
    , "div.fixed { position: fixed; z-index: 1001; width: 100%; background: rgba(255, 0, 0, 0.2); height: 2000px; }"
    , "</style>"
    , "<div class=fixed></div>"
    , "<ol>"
    , "<li>hi1</li><li>hi2</li><li>hi3</li><li>hi4</li><li>hi5</li><li>hi6</li><li>hi7</li><li>hi8</li>"
    , "<li id=target onclick=\"window.__clicked = true\">hi9</li>"
    , "<li>hi10</li><li>hi11</li><li>hi12</li><li>hi13</li><li id=li14>hi14</li>"
    , "</ol>"
    , "<script>"
    , "window.scrollTops = [];"
    , "window.addEventListener('scroll', () => { window.scrollTops.push(window.scrollY); });"
    , "</script>"
    ]

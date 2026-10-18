module Playwright.PageClickTimeoutSpec
  ( timeout1Tests
  , timeout2Tests
  , timeout3Tests
  , timeout4Tests
  ) where

import Control.Monad
  ( unless
  )
import Data.Text
  ( Text
  )
import Playwright
  ( ClickOptions (clickOptionsForce, clickOptionsTimeout)
  , SpecialJsValue (SpecialUndefined)
  , ViewportHeight (ViewportHeight)
  , ViewportSize (ViewportSize)
  , ViewportWidth (ViewportWidth)
  , clickHandleWith
  , clickWith
  , defaultClickOptions
  , evalOnSelector
  , evaluate
  , evaluateNumber
  , evaluateOnElement
  , evaluateSpecial
  , querySelector
  , selectorFromText
  , setContent
  , setViewportSize
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , locate
  , requireHandle
  , withAssetPage
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

timeout1Tests :: TestTree
timeout1Tests =
  withLongTimeout $
    testGroup
      "page-click-timeout-1"
      [ testCase "should timeout waiting for button to be enabled" $
          withChromiumPage $ \page -> do
            setContent
              page
              "<button onclick=\"javascript:window.__CLICKED=true;\" disabled><span>Click target</span></button>"
            timeout <- requireTimeout 3000
            assertProtocolContains
              "Timeout 3000ms exceeded"
              ( clickWith
                  (locate page "text=Click target")
                  defaultClickOptions {clickOptionsTimeout = Just timeout}
              )
            special <- evaluateSpecial page "() => window.__CLICKED"
            assertEqual "not clicked" SpecialUndefined special
      ]

timeout2Tests :: TestTree
timeout2Tests =
  withLongTimeout $
    testGroup
      "page-click-timeout-2"
      [ testCase "should timeout waiting for display:none to be gone" $
          hiddenButtonTimeout "b => { b.style.display = 'none'; }"
      , testCase "should timeout waiting for visibility:hidden to be gone" $
          hiddenButtonTimeout "b => { b.style.visibility = 'hidden'; }"
      ]

timeout3Tests :: TestTree
timeout3Tests =
  withLongTimeout $
    testGroup
      "page-click-timeout-3"
      [ testCase "should timeout waiting for hit target" $
          withAssetPage "input/button.html" $ \page -> do
            button <- requireHandle =<< querySelector page (selectorFromText "button")
            _ <-
              evaluate
                page
                "() => { document.body.style.position = 'relative'; const blocker = document.createElement('div'); blocker.id = 'blocker'; blocker.style.position = 'absolute'; blocker.style.width = '400px'; blocker.style.height = '20px'; blocker.style.left = '0'; blocker.style.top = '0'; document.body.appendChild(blocker); }"
            timeout <- requireTimeout 5000
            assertProtocolContains
              "Timeout 5000ms exceeded"
              ( clickHandleWith
                  button
                  defaultClickOptions {clickOptionsTimeout = Just timeout}
              )
      , testCase "should still click when force but hit target is obscured" $
          withAssetPage "input/button.html" $ \page -> do
            button <- requireHandle =<< querySelector page (selectorFromText "button")
            _ <-
              evaluate
                page
                "() => { document.body.style.position = 'relative'; const blocker = document.createElement('div'); blocker.id = 'blocker'; blocker.style.position = 'absolute'; blocker.style.width = '400px'; blocker.style.height = '200px'; blocker.style.left = '0'; blocker.style.top = '0'; document.body.appendChild(blocker); }"
            clickHandleWith
              button
              defaultClickOptions {clickOptionsForce = Just True}
      , testCase "should report wrong hit target subtree" $
          withAssetPage "input/button.html" $ \page -> do
            button <- requireHandle =<< querySelector page (selectorFromText "button")
            _ <-
              evaluate
                page
                "() => { document.body.style.position = 'relative'; const blocker = document.createElement('div'); blocker.id = 'blocker'; blocker.style.position = 'absolute'; blocker.style.width = '400px'; blocker.style.height = '20px'; blocker.style.left = '0'; blocker.style.top = '0'; document.body.appendChild(blocker); const inner = document.createElement('div'); inner.id = 'inner'; inner.style.position = 'absolute'; inner.style.left = '0'; inner.style.top = '0'; inner.style.right = '0'; inner.style.bottom = '0'; blocker.appendChild(inner); }"
            timeout <- requireTimeout 5000
            assertProtocolContains
              "Timeout 5000ms exceeded"
              ( clickHandleWith
                  button
                  defaultClickOptions {clickOptionsTimeout = Just timeout}
              )
      ]

timeout4Tests :: TestTree
timeout4Tests =
  withLongTimeout $
    testGroup
      "page-click-timeout-4"
      [ testCase "should timeout waiting for stable position" $
          withAssetPage "input/button.html" $ \page -> do
            button <- requireHandle =<< querySelector page (selectorFromText "button")
            _ <-
              evaluateOnElement
                button
                "button => { button.style.transition = 'margin 5s linear 0s'; button.style.marginLeft = '200px'; }"
            timeout <- requireTimeout 3000
            assertProtocolContains
              "Timeout 3000ms exceeded"
              ( clickHandleWith
                  button
                  defaultClickOptions {clickOptionsTimeout = Just timeout}
              )
      , testCase "should fail to click the button behind a large header after scrolling around" $
          withChromiumPage $ \page -> do
            setViewportSize page (ViewportSize (ViewportWidth 500) (ViewportHeight 240))
            setContent page largeHeaderHtml
            _ <- evalOnSelector page (selectorFromText "#li14") "e => e.scrollIntoView()"
            timeout <- requireTimeout 3500
            assertProtocolContains
              "Timeout 3500ms exceeded"
              ( clickWith
                  (locate page "#target")
                  defaultClickOptions {clickOptionsTimeout = Just timeout}
              )
            special <- evaluateSpecial page "() => window.__clicked"
            assertEqual "not clicked" SpecialUndefined special
            distinct <- evaluateNumber page "() => new Set(window.scrollTops).size"
            unless (distinct > 2) $
              assertFailure ("expected more than 2 scroll positions, got " <> show distinct)
      ]

hiddenButtonTimeout :: Text -> IO ()
hiddenButtonTimeout hide =
  withAssetPage "input/button.html" $ \page -> do
    _ <- evalOnSelector page (selectorFromText "button") hide
    timeout <- requireTimeout 5000
    assertProtocolContains
      "Timeout 5000ms exceeded"
      ( clickWith
          (locate page "button")
          defaultClickOptions {clickOptionsTimeout = Just timeout}
      )

withLongTimeout :: TestTree -> TestTree
withLongTimeout =
  localOption (mkTimeout 30000000)

largeHeaderHtml :: Text
largeHeaderHtml =
  "<style>* { padding: 0; margin: 0; } li { height: 80px; border: 1px solid black; } ol { padding-top: 160px; } div.fixed { position: fixed; z-index: 1001; width: 100%; background: rgba(255, 0, 0, 0.2); height: 2000px; }</style><div class=fixed></div><ol><li>hi1</li><li>hi2</li><li>hi3</li><li>hi4</li><li>hi5</li><li>hi6</li><li>hi7</li><li>hi8</li><li id=target onclick=\"window.__clicked = true\">hi9</li><li>hi10</li><li>hi11</li><li>hi12</li><li>hi13</li><li id=li14>hi14</li></ol><script>window.scrollTops = []; window.addEventListener('scroll', () => { window.scrollTops.push(window.scrollY); });</script>"

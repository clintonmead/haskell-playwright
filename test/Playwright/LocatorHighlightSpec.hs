module Playwright.LocatorHighlightSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( BoundingBox (BoundingBox, boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  , CssPixel (CssPixel, cssPixelToScientific)
  , Page
  , Playwright
  , WaitForState (WaitForVisible)
  , defaultGotoOptions
  , evaluateText
  , goto
  , highlight
  , highlightWithStyle
  , hideHighlight
  , isVisible
  , locatorBoundingBox
  , pageHideHighlight
  , setContent
  , textContent
  , urlFromText
  , waitFor
  )
import Playwright.Harness
  ( locate
  , withChromiumPage
  , assertEqual
  , assertFailure
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
    "locator-highlight"
    [ testCase "should highlight locator" $
        withChromiumPage $ \page -> do
          setContent page "<input type='text' />"
          highlight (locate page "input")
          waitFor (locate page "x-pw-tooltip") WaitForVisible
          tooltip <- textContent (locate page "x-pw-tooltip")
          assertEqual "tooltip" (Just "locator('input')") tooltip
          visible <- isVisible (locate page "x-pw-highlight")
          assertEqual "highlight" True visible
          box1 <- requireBox =<< locatorBoundingBox (locate page "input")
          box2 <- requireBox =<< locatorBoundingBox (locate page "x-pw-highlight")
          assertEqual "box" (roundBox box1) (roundBox box2)
    , testCase "highlight should accept a CSS string style" $
        withChromiumPage $ \page -> do
          setContent page "<button>Click me</button>"
          highlightWithStyle
            (locate page "button")
            "outline: 3px solid rgb(255, 0, 0); background-color: rgba(0, 255, 0, 0.25)"
          waitFor (locate page "x-pw-highlight") WaitForVisible
          outline <- highlightProp page "outline"
          assertEqual "outline" "rgb(255, 0, 0) solid 3px" outline
          background <- highlightProp page "backgroundColor"
          assertEqual "background" "rgba(0, 255, 0, 0.25)" background
    , testCase "hideHighlight removes a styled highlight" $
        withChromiumPage $ \page -> do
          setContent page "<button>Click me</button>"
          let button = locate page "button"
          highlightWithStyle button "outline: 2px solid red"
          waitFor (locate page "x-pw-highlight") WaitForVisible
          hideHighlight button
          assertEqual "count" "0" =<< highlightCount page
    , testCase "highlight should survive navigation" $
        withChromiumPage $ \page -> do
          setContent page "<button onclick=\"console.log(1)\">Clicker</button>"
          highlight (locate page "button")
          waitFor (locate page "x-pw-highlight") WaitForVisible
          assertEqual "count before" "1" =<< highlightCount page
          _ <-
            goto
              page
              (urlFromText "data:text/html,<button>Click</button>")
              defaultGotoOptions
          -- Highlights are resolved again after the navigation.
          waitFor (locate page "x-pw-highlight") WaitForVisible
          assertEqual "count after" "1" =<< highlightCount page
          pageHideHighlight page
          assertEqual "count hidden" "0" =<< highlightCount page
    , testCase "Page.hideHighlight clears all locator highlights" $
        withChromiumPage $ \page -> do
          setContent page "<button id=one>One</button><button id=two>Two</button>"
          highlight (locate page "#one")
          highlight (locate page "#two")
          waitFor (locate page "x-pw-highlight >> nth=0") WaitForVisible
          assertEqual "count before" "2" =<< highlightCount page
          pageHideHighlight page
          assertEqual "count hidden" "0" =<< highlightCount page
    ]

-- Highlight elements live inside shadow roots, so walk them explicitly.
highlightCount :: Page -> Playwright Text
highlightCount page =
  evaluateText page $
    "() => { let n = 0; const walk = (root) => { for (const el of root.querySelectorAll('*')) {"
      <> " if (el.tagName === 'X-PW-HIGHLIGHT') n++; if (el.shadowRoot) walk(el.shadowRoot); } };"
      <> " walk(document); return String(n); }"

highlightProp :: Page -> Text -> Playwright Text
highlightProp page prop =
  evaluateText page $
    "() => { const find = (root) => { for (const el of root.querySelectorAll('*')) {"
      <> " if (el.tagName === 'X-PW-HIGHLIGHT') return el; if (el.shadowRoot) {"
      <> " const f = find(el.shadowRoot); if (f) return f; } } return null; };"
      <> " const h = find(document); return h ? h.style['" <> prop <> "'] : 'missing'; }"

requireBox :: Maybe BoundingBox -> Playwright BoundingBox
requireBox found =
  case found of
    Just box ->
      pure box
    Nothing ->
      assertFailure "missing bounding box"

roundBox :: BoundingBox -> BoundingBox
roundBox box =
  BoundingBox
    { boundingBoxX = roundPixel (boundingBoxX box)
    , boundingBoxY = roundPixel (boundingBoxY box)
    , boundingBoxWidth = roundPixel (boundingBoxWidth box)
    , boundingBoxHeight = roundPixel (boundingBoxHeight box)
    }

roundPixel :: CssPixel -> CssPixel
roundPixel pixel =
  CssPixel (fromInteger (round (cssPixelToScientific pixel)))

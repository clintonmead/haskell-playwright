module Playwright.PageClickScrollSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , replace
  )
import Playwright
  ( AriaRole (AriaRole)
  , Playwright
  , ClickOptions (clickOptionsForce, clickOptionsScroll, clickOptionsTimeout)
  , HoverOptions (hoverOptionsScroll, hoverOptionsTimeout)
  , ScrollOption (ScrollNone)
  , click
  , clickWith
  , defaultClickOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , defaultHoverOptions
  , evaluateBool
  , evaluateNumber
  , frameLocator
  , goto
  , hoverWith
  , locatorScrollIntoView
  , selectorFromText
  , setContent
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError
  )
import UnliftIO.Exception
  ( try
  )
import Playwright.Harness
  ( requireTimeout
  , assertProtocolContains
  , locate
  , withAssets
  , withChromiumPage
  , assertBool
  , assertEqual
  )
import Playwright.Locator
  ( getByRole
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
    "page-click-scroll"
    [ testCase "should not hit scroll bar" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>.categories { width: 180px; display: flex; overflow-x: scroll; } button { flex: none; height: 28px; }</style><div class=\"categories\"><button>One</button><button>Two</button><button>Three</button><button>Story</button><button>More</button><button>Items</button><button>Here</button></div>"
          timeout <- requireTimeout 2000
          clickWith
            (locate page "text=Story")
            defaultClickOptions {clickOptionsTimeout = Just timeout}
    , testCase "should scroll into view display:contents" $
        clickDisplayContents
          "<div style=\"background:red;height:2000px\">filler</div><div>Example text, and button here: <button style=\"display: contents\" onclick=\"window._clicked=true;\">click me</button></div>"
    , testCase "should scroll into view display:contents with a child" $
        clickDisplayContents
          "<div style=\"background:red;height:2000px\">filler</div>Example text, and button here: <button style=\"display: contents\" onclick=\"window._clicked=true;\"><div>click me</div></button>"
    , testCase "should not crash when force-clicking hidden input" $
        withChromiumPage $ \page -> do
          setContent page "<input type=hidden>"
          timeout <- requireTimeout 2000
          assertProtocolContains
            "Element is not visible"
            ( clickWith
                (locate page "input")
                defaultClickOptions
                  { clickOptionsForce = Just True
                  , clickOptionsTimeout = Just timeout
                  }
            )
    , testCase "should scroll into view span element" $
        withChromiumPage $ \page -> do
          setContent page "<div id=big style=\"height: 10000px;\"></div><span id=small>foo</span>"
          locatorScrollIntoView (locate page "#small")
          scrollY <- evaluateNumber page "() => window.scrollY"
          assertBool "scrolled" (scrollY > 9000)
    , testCase "should scroll into view element in iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let
            cross = replace "127.0.0.1" "localhost" prefix
          setContent
            page
            ( "<div id=big style=\"height: 10000px;\"></div><iframe src='"
                <> cross
                <> "/input/button.html'></iframe>"
            )
          timeout <- requireTimeout 5000
          clickWith
            ( getByRole
                (frameLocator page (selectorFromText "iframe"))
                (AriaRole "button")
                defaultGetByRoleOptions
            )
            defaultClickOptions {clickOptionsTimeout = Just timeout}
    , -- Skipped: upstream marks "should scroll into view display:contents with position" as fixme on
      -- Chromium (DOM.getBoxModel does not work for display:contents), and this suite runs on Chromium only.
      testCase "should not scroll the page when scroll is \"none\"" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div style=\"height: 2000px;\"></div><button onclick=\"window._clicked=true\">click me</button>"
          timeout <- requireTimeout 2000
          assertProtocolContains
            "element is outside of the viewport"
            ( clickWith
                (locate page "button")
                defaultClickOptions {clickOptionsScroll = Just ScrollNone, clickOptionsTimeout = Just timeout}
            )
          assertEqual "not clicked" False =<< evaluateBool page "() => !!window._clicked"
          assertEqual "not scrolled" (0 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "should click in-viewport element when scroll is \"none\"" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<button onclick=\"window._clicked=true\">click me</button><div style=\"height: 2000px;\"></div>"
          timeout <- requireTimeout 2000
          clickWith
            (locate page "button")
            defaultClickOptions {clickOptionsScroll = Just ScrollNone, clickOptionsTimeout = Just timeout}
          assertEqual "clicked" True =<< evaluateBool page "() => !!window._clicked"
          assertEqual "not scrolled" (0 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "should not scroll nested container when scroll is \"none\"" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div style=\"height: 100px; width: 100px; overflow-y: scroll;\"><div style=\"height: 50px;\">A</div><div style=\"height: 50px;\">B</div><button style=\"height: 50px; width: 100px;\" onclick=\"window._clicked=true\">C</button></div>"
          timeout <- requireTimeout 2000
          result <-
            try
              ( clickWith
                  (locate page "button")
                  defaultClickOptions {clickOptionsScroll = Just ScrollNone, clickOptionsTimeout = Just timeout}
              )
              :: Playwright (Either PlaywrightError ())
          assertEqual "failed without scroll" True (either (const True) (const False) result)
          assertEqual "not clicked" False =<< evaluateBool page "() => !!window._clicked"
          clickWith
            (locate page "button")
            defaultClickOptions {clickOptionsTimeout = Just timeout}
          assertEqual "clicked with default scroll" True =<< evaluateBool page "() => !!window._clicked"
    , testCase "should not scroll on hover when scroll is \"none\"" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div style=\"height: 2000px;\"></div><div onmouseover=\"window._hovered=true\" style=\"width: 50px; height: 50px;\">hover me</div>"
          timeout <- requireTimeout 2000
          assertProtocolContains
            "element is outside of the viewport"
            ( hoverWith
                (locate page "div >> text=hover me")
                defaultHoverOptions {hoverOptionsScroll = Just ScrollNone, hoverOptionsTimeout = Just timeout}
            )
          assertEqual "not hovered" False =<< evaluateBool page "() => !!window._hovered"
          assertEqual "not scrolled" (0 :: Double) =<< evaluateNumber page "() => window.scrollY"
    , testCase "should scroll instantly on retry when scroll-behavior is smooth" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>html { scroll-behavior: smooth; } body { margin: 0; } .spacer { height: 2000px; } #header { position: fixed; top: 0; left: 0; right: 0; height: calc(100vh - 50px); background: rgba(0, 0, 0, 0.1); } button { height: 30px; }</style><div id=\"header\"></div><div class=\"spacer\"></div><button onclick=\"window.clicked = true\">Target</button><div class=\"spacer\"></div><script>window.scrolls = []; addEventListener('scroll', () => window.scrolls.push(window.scrollY));</script>"
          click (locate page "button")
          assertEqual "clicked" True =<< evaluateBool page "() => !!window.clicked"
          assertBool "at most two scrolls" . (<= (2 :: Double)) =<< evaluateNumber page "() => window.scrolls.length"
    ]

clickDisplayContents :: Text -> IO ()
clickDisplayContents html =
  withChromiumPage $ \page -> do
    setContent page html
    timeout <- requireTimeout 5000
    clickWith
      (locate page "text=click me")
      defaultClickOptions {clickOptionsTimeout = Just timeout}
    assertEqual "clicked" True =<< evaluateBool page "() => window._clicked"


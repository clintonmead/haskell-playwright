module Playwright.PageClickScrollSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , replace
  )
import Playwright
  ( AriaRole (AriaRole)
  , ClickOptions (clickOptionsForce, clickOptionsTimeout)
  , Timeout
  , clickWith
  , defaultClickOptions
  , defaultGetByRoleOptions
  , defaultGotoOptions
  , evaluateBool
  , evaluateNumber
  , frameLocator
  , goto
  , locatorScrollIntoView
  , milliseconds
  , selectorFromText
  , setContent
  , urlFromText
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssets
  , withChromiumPage
  )
import Playwright.Locator
  ( getByRole
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  , testCase
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

requireTimeout :: Double -> IO Timeout
requireTimeout n =
  case milliseconds n of
    Nothing ->
      assertFailure ("milliseconds " <> show n)
    Just timeout ->
      pure timeout

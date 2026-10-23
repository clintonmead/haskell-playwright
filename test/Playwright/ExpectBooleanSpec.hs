module Playwright.ExpectBooleanSpec
  ( tests
  ) where

import Playwright
  ( Locator
  , Playwright
  , expectIsNot
  , expectTimeout
  , defaultExpectOptions
  , expectLocator
  , expectLocatorWith
  , setContent
  )
import Data.Text
  ( Text
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertEqual
  , locate
  , withChromiumPage
  )
import Playwright.Timeout
  ( milliseconds
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( try
  )

tests :: TestTree
tests =
  testGroup
    "expect-boolean"
    [ testGroup
        "toBeVisible"
        [ testCase "default" $
            withChromiumPage $ \page -> do
              setContent page "<input></input>"
              expectLocator (locate page "input") "to.be.visible"
        , testCase "with not" $
            withChromiumPage $ \page -> do
              setContent page "<button style=\"display: none\">hello</button>"
              notExpect (locate page "button") "to.be.visible"
        , testCase "with visible:true" $
            withChromiumPage $ \page -> do
              setContent page "<button>hello</button>"
              expectLocator (locate page "button") "to.be.visible"
        , testCase "with visible:false" $
            withChromiumPage $ \page -> do
              setContent page "<button hidden>hello</button>"
              expectLocator (locate page "button") "to.be.hidden"
        , testCase "with not and visible:false" $
            withChromiumPage $ \page -> do
              setContent page "<button>hello</button>"
              notExpect (locate page "button") "to.be.hidden"
        , testCase "eventually" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<div></div><script>setTimeout(() => document.querySelector('div').innerHTML = '<span>Hello</span>', 100)</script>"
              expectLocator (locate page "span") "to.be.visible"
        , testCase "eventually with not" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<div><span>Hello</span></div><script>setTimeout(() => document.querySelector('span').textContent = '', 100)</script>"
              notExpect (locate page "span") "to.be.visible"
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<button style=\"display: none\"></button>"
              failsWithin 1000 (locate page "button") "to.be.visible"
        , testCase "fail with not" $
            withChromiumPage $ \page -> do
              setContent page "<input></input>"
              failsWithinNot 1000 (locate page "input") "to.be.visible"
        , testCase "with impossible timeout" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              expectWithin 1 (locate page "#node") "to.be.visible"
        , testCase "with impossible timeout .not" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              notExpectWithin 1 (locate page "no-such-thing") "to.be.visible"
        ]
    , testGroup
        "toBeAttached"
        [ testCase "default" $
            withChromiumPage $ \page -> do
              setContent page "<input></input>"
              expectLocator (locate page "input") "to.be.attached"
        , testCase "with hidden element" $
            withChromiumPage $ \page -> do
              setContent page "<button style=\"display:none\">hello</button>"
              expectLocator (locate page "button") "to.be.attached"
        , testCase "with not" $
            withChromiumPage $ \page -> do
              setContent page "<button>hello</button>"
              notExpect (locate page "input") "to.be.attached"
        , testCase "with attached:true" $
            withChromiumPage $ \page -> do
              setContent page "<button>hello</button>"
              expectLocator (locate page "button") "to.be.attached"
        , testCase "with attached:false" $
            withChromiumPage $ \page -> do
              setContent page "<button>hello</button>"
              expectLocator (locate page "input") "to.be.detached"
        , testCase "with not and attached:false" $
            withChromiumPage $ \page -> do
              setContent page "<button>hello</button>"
              notExpect (locate page "button") "to.be.detached"
        , testCase "eventually" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<div></div><script>setTimeout(() => document.querySelector('div').innerHTML = '<span>Hello</span>', 100)</script>"
              expectLocator (locate page "span") "to.be.attached"
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<div></div>"
              failsWithin 1000 (locate page "span") "to.be.attached"
        , testCase "with impossible timeout" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              expectWithin 1 (locate page "#node") "to.be.attached"
        , testCase "with impossible timeout .not" $
            withChromiumPage $ \page -> do
              setContent page "<div id=node>Text content</div>"
              notExpectWithin 1 (locate page "no-such-thing") "to.be.attached"
        ]
    ]

notExpect :: Locator -> Text -> Playwright ()
notExpect locator expression =
  expectLocatorWith defaultExpectOptions {expectIsNot = True} locator expression

expectWithin :: Double -> Locator -> Text -> Playwright ()
expectWithin ms locator expression =
  expectLocatorWith defaultExpectOptions {expectTimeout = milliseconds ms} locator expression

notExpectWithin :: Double -> Locator -> Text -> Playwright ()
notExpectWithin ms locator expression =
  expectLocatorWith
    defaultExpectOptions {expectIsNot = True, expectTimeout = milliseconds ms}
    locator
    expression

failsWithin :: Double -> Locator -> Text -> Playwright ()
failsWithin ms locator expression =
  assertFails (expectWithin ms locator expression)

failsWithinNot :: Double -> Locator -> Text -> Playwright ()
failsWithinNot ms locator expression =
  assertFails (notExpectWithin ms locator expression)

assertFails :: Playwright () -> Playwright ()
assertFails action = do
  outcome <- try action :: Playwright (Either PlaywrightError ())
  assertEqual "expectation should fail" True (either (const True) (const False) outcome)

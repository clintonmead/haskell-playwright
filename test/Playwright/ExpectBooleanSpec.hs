module Playwright.ExpectBooleanSpec
  ( tests
  ) where

import Playwright
  ( CheckedOptions (CheckedOptions)
  , Locator
  , Playwright
  , defaultCheckedOptions
  , expectIsNot
  , expectTimeout
  , defaultExpectOptions
  , expectLocator
  , expectLocatorChecked
  , expectLocatorEditable
  , expectLocatorEnabled
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
    , testGroup
        "toBeChecked"
        [ testCase "default" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox checked></input>"
              checkedWith defaultCheckedOptions (locate page "input")
        , testCase "with checked:true" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox checked></input>"
              checkedWith (CheckedOptions (Just True) Nothing) (locate page "input")
        , testCase "with checked:false" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox checked></input>"
              notCheckedWith (CheckedOptions (Just False) Nothing) (locate page "input")
        , testCase "with indeterminate:true" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<input type=checkbox></input><script>document.querySelector('input').indeterminate = true</script>"
              checkedWith (CheckedOptions Nothing (Just True)) (locate page "input")
        , testCase "with indeterminate:true and checked" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<input type=checkbox></input><script>document.querySelector('input').indeterminate = true</script>"
              failsWithinChecked 1000 (CheckedOptions (Just False) (Just True)) (locate page "input")
        , testCase "fail" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox></input>"
              failsWithinChecked 1000 defaultCheckedOptions (locate page "input")
        , testCase "with not" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox></input>"
              notCheckedWith defaultCheckedOptions (locate page "input")
        , testCase "fail with not" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox checked></input>"
              failsWithinNotChecked 1000 defaultCheckedOptions (locate page "input")
        , testCase "fail with checked:false" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox checked></input>"
              failsWithinChecked 1000 (CheckedOptions (Just False) Nothing) (locate page "input")
        , testCase "fail with indeterminate: true" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox></input>"
              failsWithinChecked 1000 (CheckedOptions Nothing (Just True)) (locate page "input")
        , testCase "with impossible timeout" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox checked></input>"
              expectCheckedWithin 1 defaultCheckedOptions (locate page "input")
        , testCase "with impossible timeout .not" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox></input>"
              notCheckedWithin 1 defaultCheckedOptions (locate page "input")
        , testCase "with value attribute" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox checked=\"yes\"></input>"
              checkedWith defaultCheckedOptions (locate page "input")
        ]
    , testGroup
        "toBeEditable"
        [ testCase "default" $
            withChromiumPage $ \page -> do
              setContent page "<input></input>"
              editableWith True (locate page "input")
        , testCase "with not" $
            withChromiumPage $ \page -> do
              setContent page "<input readonly></input>"
              notEditableWith True (locate page "input")
        , testCase "with editable:true" $
            withChromiumPage $ \page -> do
              setContent page "<input></input>"
              editableWith True (locate page "input")
        , testCase "with editable:false" $
            withChromiumPage $ \page -> do
              setContent page "<input readonly></input>"
              editableWith False (locate page "input")
        , testCase "with not and editable:false" $
            withChromiumPage $ \page -> do
              setContent page "<input></input>"
              notEditableWith False (locate page "input")
        , testCase "throws" $
            withChromiumPage $ \page -> do
              setContent page "<button>"
              failsWithin 1000 (locate page "button") "to.be.editable"
        ]
    , testGroup
        "toBeEnabled"
        [ testCase "default" $
            withChromiumPage $ \page -> do
              setContent page "<button>Text</button>"
              enabledWith True (locate page "button")
        , testCase "with enabled:true" $
            withChromiumPage $ \page -> do
              setContent page "<button>Text</button>"
              enabledWith True (locate page "button")
        , testCase "with enabled:false" $
            withChromiumPage $ \page -> do
              setContent page "<button disabled>Text</button>"
              enabledWith False (locate page "button")
        , testCase "failed" $
            withChromiumPage $ \page -> do
              setContent page "<button disabled>Text</button>"
              failsWithin 1000 (locate page "button") "to.be.enabled"
        , testCase "eventually" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<button disabled>Text</button><script>setTimeout(() => document.querySelector('button').removeAttribute('disabled'), 500)</script>"
              enabledWith True (locate page "button")
        , testCase "eventually with not" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<button>Text</button><script>setTimeout(() => document.querySelector('button').setAttribute('disabled', ''), 500)</script>"
              notEnabledWith True (locate page "button")
        , testCase "with not and enabled:false" $
            withChromiumPage $ \page -> do
              setContent page "<button>Text</button>"
              notEnabledWith False (locate page "button")
        , testCase "toBeDisabled" $
            withChromiumPage $ \page -> do
              setContent page "<button disabled>Text</button>"
              expectLocator (locate page "button") "to.be.disabled"
        ]
    , testGroup
        "toBeEmpty and toBeDisabled"
        [ testCase "toBeEmpty input" $
            withChromiumPage $ \page -> do
              setContent page "<input></input>"
              expectLocator (locate page "input") "to.be.empty"
        , testCase "not.toBeEmpty" $
            withChromiumPage $ \page -> do
              setContent page "<input value=text></input>"
              notExpect (locate page "input") "to.be.empty"
        , testCase "toBeEmpty div" $
            withChromiumPage $ \page -> do
              setContent page "<div style=\"width: 50; height: 50px\"></div>"
              expectLocator (locate page "div") "to.be.empty"
        , testCase "toBeDisabled with value" $
            withChromiumPage $ \page -> do
              setContent page "<button disabled=\"yes\">Text</button>"
              expectLocator (locate page "button") "to.be.disabled"
        , testCase "toBeHidden with value" $
            withChromiumPage $ \page -> do
              setContent page "<input type=checkbox hidden=\"of course\"></input>"
              expectLocator (locate page "input") "to.be.hidden"
        , testCase "not.toBeDisabled div" $
            withChromiumPage $ \page -> do
              setContent page "<div></div>"
              notExpect (locate page "div") "to.be.disabled"
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

checkedWith :: CheckedOptions -> Locator -> Playwright ()
checkedWith options =
  expectLocatorChecked defaultExpectOptions options

notCheckedWith :: CheckedOptions -> Locator -> Playwright ()
notCheckedWith options =
  expectLocatorChecked defaultExpectOptions {expectIsNot = True} options

expectCheckedWithin :: Double -> CheckedOptions -> Locator -> Playwright ()
expectCheckedWithin ms options =
  expectLocatorChecked defaultExpectOptions {expectTimeout = milliseconds ms} options

notCheckedWithin :: Double -> CheckedOptions -> Locator -> Playwright ()
notCheckedWithin ms options =
  expectLocatorChecked
    defaultExpectOptions {expectIsNot = True, expectTimeout = milliseconds ms}
    options

failsWithinChecked :: Double -> CheckedOptions -> Locator -> Playwright ()
failsWithinChecked ms options locator =
  assertFails (expectCheckedWithin ms options locator)

failsWithinNotChecked :: Double -> CheckedOptions -> Locator -> Playwright ()
failsWithinNotChecked ms options locator =
  assertFails (notCheckedWithin ms options locator)

editableWith :: Bool -> Locator -> Playwright ()
editableWith editable =
  expectLocatorEditable defaultExpectOptions editable

notEditableWith :: Bool -> Locator -> Playwright ()
notEditableWith editable =
  expectLocatorEditable defaultExpectOptions {expectIsNot = True} editable

enabledWith :: Bool -> Locator -> Playwright ()
enabledWith enabled =
  expectLocatorEnabled defaultExpectOptions enabled

notEnabledWith :: Bool -> Locator -> Playwright ()
notEnabledWith enabled =
  expectLocatorEnabled defaultExpectOptions {expectIsNot = True} enabled

assertFails :: Playwright () -> Playwright ()
assertFails action = do
  outcome <- try action :: Playwright (Either PlaywrightError ())
  assertEqual "expectation should fail" True (either (const True) (const False) outcome)

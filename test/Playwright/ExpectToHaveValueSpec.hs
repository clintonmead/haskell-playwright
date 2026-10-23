-- | Ports of upstream tests/page/expect-to-have-value.spec.ts.
--
-- Failure cases assert that the matcher throws. The upstream assertions on
-- the rendered failure message are not ported, because the Haskell client
-- does not decode the driver's error details or formatted message into the
-- exception.
module Playwright.ExpectToHaveValueSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import Data.Text
  ( Text
  )
import Playwright
  ( ExpectOptions (expectTimeout)
  , ExpectedText (ExpectedRegex, ExpectedString)
  , Playwright
  , SelectOption (SelectValue)
  , defaultExpectOptions
  , expectHaveValue
  , expectHaveValues
  , fill
  , selectOption
  , setContent
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertBool
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
    "expect-to-have-value"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          setContent page "<input id=node></input>"
          let node = locate page "#node"
          fill node "Text content"
          expectHaveValue defaultExpectOptions node (ExpectedString "Text content")
    , testCase "should work with label" $
        withChromiumPage $ \page -> do
          setContent page "<label><input></input></label>"
          fill (locate page "label input") "Text content"
          expectHaveValue defaultExpectOptions (locate page "label") (ExpectedString "Text content")
    , testCase "should work with regex" $
        withChromiumPage $ \page -> do
          setContent page "<input id=node></input>"
          let node = locate page "#node"
          fill node "Text content"
          expectHaveValue defaultExpectOptions node (rx "Text" "")
    , testCase "should support failure" $
        withChromiumPage $ \page -> do
          setContent page "<input id=node></input>"
          let node = locate page "#node"
          fill node "Text content"
          assertFails $
            expectHaveValue (timeoutOpts 1000) node (rx "Text2" "")
    , testGroup
        "toHaveValues with multi-select"
        [ testCase "works with text" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<select multiple>\
                \<option value=\"R\">Red</option>\
                \<option value=\"G\">Green</option>\
                \<option value=\"B\">Blue</option>\
                \</select>"
              let select = locate page "select"
              void (selectOption select [SelectValue "R", SelectValue "G"])
              expectHaveValues defaultExpectOptions select [ExpectedString "R", ExpectedString "G"]
        , testCase "follows labels" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<label for=\"colors\">Pick a Color</label>\
                \<select id=\"colors\" multiple>\
                \<option value=\"R\">Red</option>\
                \<option value=\"G\">Green</option>\
                \<option value=\"B\">Blue</option>\
                \</select>"
              let label = locate page "text=Pick a Color"
              void (selectOption label [SelectValue "R", SelectValue "G"])
              expectHaveValues defaultExpectOptions label [ExpectedString "R", ExpectedString "G"]
        , testCase "exact match with text failure" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<select multiple>\
                \<option value=\"RR\">Red</option>\
                \<option value=\"GG\">Green</option>\
                \</select>"
              let select = locate page "select"
              void (selectOption select [SelectValue "RR", SelectValue "GG"])
              assertFails $
                expectHaveValues (timeoutOpts 1000) select [ExpectedString "R", ExpectedString "G"]
        , testCase "works with regex" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<select multiple>\
                \<option value=\"R\">Red</option>\
                \<option value=\"G\">Green</option>\
                \<option value=\"B\">Blue</option>\
                \</select>"
              let select = locate page "select"
              void (selectOption select [SelectValue "R", SelectValue "G"])
              expectHaveValues defaultExpectOptions select [rx "R" "", rx "G" ""]
        , testCase "fails when items not selected" $
            withChromiumPage $ \page -> do
              setContent
                page
                "<select multiple>\
                \<option value=\"R\">Red</option>\
                \<option value=\"G\">Green</option>\
                \<option value=\"B\">Blue</option>\
                \</select>"
              let select = locate page "select"
              void (selectOption select [SelectValue "B"])
              assertFails $
                expectHaveValues (timeoutOpts 1000) select [rx "R" "", rx "G" ""]
        , testCase "fails when multiple not specified" $
            withChromiumPage $ \page -> do
              -- The upstream assertion checks the driver's "Not a select element
              -- with a multiple attribute" message, which is not ported.
              setContent
                page
                "<select>\
                \<option value=\"R\">Red</option>\
                \<option value=\"G\">Green</option>\
                \<option value=\"B\">Blue</option>\
                \</select>"
              let select = locate page "select"
              void (selectOption select [SelectValue "B"])
              assertFails $
                expectHaveValues (timeoutOpts 1000) select [rx "R" "", rx "G" ""]
        , testCase "fails when not a select element" $
            withChromiumPage $ \page -> do
              setContent page "<input value=\"foo\" />"
              assertFails $
                expectHaveValues (timeoutOpts 1000) (locate page "input") [rx "R" "", rx "G" ""]
        ]
    ]

rx :: Text -> Text -> ExpectedText
rx = ExpectedRegex

timeoutOpts :: Double -> ExpectOptions
timeoutOpts ms = defaultExpectOptions {expectTimeout = milliseconds ms}

-- | Run a matcher that is expected to throw. The matcher's own failure
-- message is not checked; see the module header.
assertFails :: Playwright () -> Playwright ()
assertFails action = do
  outcome <- try action :: Playwright (Either PlaywrightError ())
  case outcome of
    Left _ -> pure ()
    Right _ -> assertBool "expectation should have failed" False

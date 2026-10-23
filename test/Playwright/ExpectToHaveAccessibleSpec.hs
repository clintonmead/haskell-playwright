module Playwright.ExpectToHaveAccessibleSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( AriaRole (AriaRole)
  , Exactness (Inexact)
  , ExpectOptions (expectIsNot)
  , Locator
  , Page
  , Playwright
  , TextExpectation (ExpectRegex, ExpectString)
  , TextMatchOptions (textMatchIgnoreCase)
  , TextProperty (AccessibleDescription, AccessibleErrorMessage, AccessibleName)
  , defaultExpectOptions
  , defaultTextMatchOptions
  , expectLocatorRole
  , expectLocatorText
  , expectLocatorTextWith
  , fill
  , getByLabel
  , setContent
  )
import Playwright.Harness
  ( locate
  , withChromiumPage
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
    "expect-to-have-accessible"
    [ testCase "toHaveAccessibleName" $
        withChromiumPage $ \page -> do
          setContent page "<div role=\"button\" aria-label=\"Hello\"></div>"
          let box = locate page "div"
          expectText box AccessibleName (str "Hello")
          notExpectText box AccessibleName (str "hello")
          ignoreCaseExpectText box AccessibleName (str "hello")
          expectText box AccessibleName (regex "ell\\w")
          notExpectText box AccessibleName (regex "hello")
          ignoreCaseExpectText box AccessibleName (regex "hello")

          setContent page "<button>foo&nbsp;bar\nbaz</button>"
          expectText (locate page "button") AccessibleName (str "foo bar baz")

          setContent
            page
            "<select>\n      <option>&nbsp;HTML</option>\n    </select>"
          expectText (locate page "option") AccessibleName (str "HTML")
    , testCase "toHaveAccessibleDescription" $
        withChromiumPage $ \page -> do
          setContent page "<div role=\"button\" aria-description=\"Hello\"></div>"
          let box = locate page "div"
          expectText box AccessibleDescription (str "Hello")
          notExpectText box AccessibleDescription (str "hello")
          ignoreCaseExpectText box AccessibleDescription (str "hello")
          expectText box AccessibleDescription (regex "ell\\w")
          notExpectText box AccessibleDescription (regex "hello")
          ignoreCaseExpectText box AccessibleDescription (regex "hello")

          setContent
            page
            "<div role=\"button\" aria-describedby=\"desc\"></div>\n<span id=\"desc\">foo&nbsp;bar\nbaz</span>"
          expectText (locate page "div") AccessibleDescription (str "foo bar baz")
    , testCase "toHaveAccessibleErrorMessage" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<form>\n\
            \  <input role=\"textbox\" aria-invalid=\"true\" aria-errormessage=\"error-message\" />\n\
            \  <div id=\"error-message\">Hello</div>\n\
            \  <div id=\"irrelevant-error\">This should not be considered.</div>\n\
            \</form>"
          let input = locate page "input[role=\"textbox\"]"
          expectText input AccessibleErrorMessage (str "Hello")
          notExpectText input AccessibleErrorMessage (str "hello")
          ignoreCaseExpectText input AccessibleErrorMessage (str "hello")
          expectText input AccessibleErrorMessage (regex "ell\\w")
          notExpectText input AccessibleErrorMessage (regex "hello")
          ignoreCaseExpectText input AccessibleErrorMessage (regex "hello")
          notExpectText input AccessibleErrorMessage (str "This should not be considered.")
    , testCase "toHaveAccessibleErrorMessage should handle multiple aria-errormessage references" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<form>\n\
            \  <input role=\"textbox\" aria-invalid=\"true\" aria-errormessage=\"error1 error2\" />\n\
            \  <div id=\"error1\">First error message.</div>\n\
            \  <div id=\"error2\">Second error message.</div>\n\
            \  <div id=\"irrelevant-error\">This should not be considered.</div>\n\
            \</form>"
          let input = locate page "input[role=\"textbox\"]"
          expectText input AccessibleErrorMessage (str "First error message. Second error message.")
          expectText input AccessibleErrorMessage (regexI "first error message.")
          expectText input AccessibleErrorMessage (regexI "second error message.")
          notExpectText input AccessibleErrorMessage (regexI "This should not be considered.")
    , testGroup
        "toHaveAccessibleErrorMessage should handle aria-invalid attribute"
        [ testGroup
            "evaluated in false"
            [ testCase "no aria-invalid attribute" $
                withChromiumPage $ \page -> do
                  input <- invalidFieldPage page Nothing
                  notExpectText input AccessibleErrorMessage (str errorMessageText)
            , testCase "aria-invalid=\"false\"" $
                withChromiumPage $ \page -> do
                  input <- invalidFieldPage page (Just "false")
                  notExpectText input AccessibleErrorMessage (str errorMessageText)
            , testCase "aria-invalid=\"\" (empty string)" $
                withChromiumPage $ \page -> do
                  input <- invalidFieldPage page (Just "")
                  notExpectText input AccessibleErrorMessage (str errorMessageText)
            ]
        , testGroup
            "evaluated in true"
            [ testCase "aria-invalid=\"true\"" $
                withChromiumPage $ \page -> do
                  input <- invalidFieldPage page (Just "true")
                  expectText input AccessibleErrorMessage (str errorMessageText)
            , testCase "aria-invalid=\"foo\" (unrecognized value)" $
                withChromiumPage $ \page -> do
                  input <- invalidFieldPage page (Just "foo")
                  expectText input AccessibleErrorMessage (str errorMessageText)
            ]
        ]
    , testCase "should show error message when validity is false and aria-invalid is true" $
        withChromiumPage $ \page -> do
          rangeFieldPage page "true"
          let input = locate page "#node"
          fill input "101"
          expectText input AccessibleErrorMessage (str errorMessageText)
    , testCase "should show error message when validity is true and aria-invalid is true" $
        withChromiumPage $ \page -> do
          rangeFieldPage page "true"
          let input = locate page "#node"
          fill input "99"
          expectText input AccessibleErrorMessage (str errorMessageText)
    , testCase "should show error message when validity is false and aria-invalid is false" $
        withChromiumPage $ \page -> do
          rangeFieldPage page "false"
          let input = locate page "#node"
          fill input "101"
          expectText input AccessibleErrorMessage (str errorMessageText)
    , testCase "should not show error message when validity is true and aria-invalid is false" $
        withChromiumPage $ \page -> do
          rangeFieldPage page "false"
          let input = locate page "#node"
          fill input "99"
          notExpectText input AccessibleErrorMessage (str errorMessageText)
    , testCase "should show error message for all roles" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<label for=\"file\">File input</label>\n\
            \<input id=\"file\" aria-invalid=\"true\" aria-errormessage=\"file-error-id\" type=\"file\" />\n\
            \<p id=\"file-error-id\" role=\"alert\">File is incorrect</p>"
          expectText
            (getByLabel page "File input" Inexact)
            AccessibleErrorMessage
            (str "File is incorrect")
    , testCase "toHaveRole" $
        withChromiumPage $ \page -> do
          setContent page "<div role=\"button\">Button!</div>"
          let box = locate page "div"
          expectLocatorRole defaultExpectOptions box (AriaRole "button")
          expectLocatorRole notExpect box (AriaRole "checkbox")
    ]

errorMessageText :: Text
errorMessageText = "Error message"

-- | A text input whose error message is shown through @aria-errormessage@,
-- with the given @aria-invalid@ value (or no attribute at all).
invalidFieldPage :: Page -> Maybe Text -> Playwright Locator
invalidFieldPage page ariaInvalid = do
  setContent
    page
    ( "<form>\n\
      \  <input id=\"node\" role=\"textbox\" "
        <> maybe "" (\value -> "aria-invalid=\"" <> value <> "\" ") ariaInvalid
        <> "aria-errormessage=\"error-msg\" />\n\
           \  <div id=\"error-msg\">"
        <> errorMessageText
        <> "</div>\n\
           \</form>"
    )
  pure (locate page "#node")

-- | A number input with a range, so that filling it out of range makes its
-- validity false.
rangeFieldPage :: Page -> Text -> Playwright ()
rangeFieldPage page ariaInvalid =
  setContent
    page
    ( "<form>\n\
      \  <input id=\"node\" role=\"textbox\" type=\"number\" min=\"1\" max=\"100\" aria-invalid=\""
        <> ariaInvalid
        <> "\" aria-errormessage=\"error-msg\" />\n\
         \  <div id=\"error-msg\">"
        <> errorMessageText
        <> "</div>\n\
         \</form>"
    )

str :: Text -> TextExpectation
str = ExpectString

regex :: Text -> TextExpectation
regex source = ExpectRegex source ""

regexI :: Text -> TextExpectation
regexI source = ExpectRegex source "i"

expectText :: Locator -> TextProperty -> TextExpectation -> Playwright ()
expectText = expectLocatorText

notExpectText :: Locator -> TextProperty -> TextExpectation -> Playwright ()
notExpectText locator property =
  expectLocatorTextWith
    defaultExpectOptions {expectIsNot = True}
    defaultTextMatchOptions
    locator
    property

ignoreCaseExpectText :: Locator -> TextProperty -> TextExpectation -> Playwright ()
ignoreCaseExpectText locator property =
  expectLocatorTextWith
    defaultExpectOptions
    defaultTextMatchOptions {textMatchIgnoreCase = True}
    locator
    property

notExpect :: ExpectOptions
notExpect = defaultExpectOptions {expectIsNot = True}

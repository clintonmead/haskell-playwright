module Playwright.PageCheckSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , unpack
  )
import Playwright
  ( CheckOptions (checkOptionsPosition, checkOptionsTrial)
  , CssPixel (CssPixel)
  , Point (Point)
  , boundingBox
  , boundingBoxWidth
  , cssPixelFromInt
  , cssPixelToScientific
  , querySelector
  , selectorFromText
  , check
  , checkWith
  , defaultCheckOptions
  , evaluateBool
  , evaluateOnLocatorBool
  , evaluateText
  , isChecked
  , setChecked
  , setContent
  , uncheck
  , uncheckWith
  )
import Playwright.Harness
  ( assertFailure
  , assertProtocolContains
  , locate
  , requireHandle
  , withChromiumPage
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
    "page-check"
    ( [ testCase "should check the box" $
          withChromiumPage $ \page -> do
            setContent page "<input id='checkbox' type='checkbox'></input>"
            check (locate page "input")
            assertEqual "checked" True =<< evaluateBool page "() => window.checkbox.checked"
      , testCase "should not check the checked box" $
          withChromiumPage $ \page -> do
            setContent page "<input id='checkbox' type='checkbox' checked></input>"
            check (locate page "input")
            assertEqual "checked" True =<< evaluateBool page "() => window.checkbox.checked"
      , testCase "should uncheck the box" $
          withChromiumPage $ \page -> do
            setContent page "<input id='checkbox' type='checkbox' checked></input>"
            uncheck (locate page "input")
            assertEqual "checked" False =<< evaluateBool page "() => window.checkbox.checked"
      , testCase "should not uncheck the unchecked box" $
          withChromiumPage $ \page -> do
            setContent page "<input id='checkbox' type='checkbox'></input>"
            uncheck (locate page "input")
            assertEqual "checked" False =<< evaluateBool page "() => window.checkbox.checked"
      , testCase "should check radio" $
          withChromiumPage $ \page -> do
            setContent
              page
              "<input type='radio'>one</input><input id='two' type='radio'>two</input><input type='radio'>three</input>"
            check (locate page "#two")
            assertEqual "checked" True =<< evaluateBool page "() => window.two.checked"
      , testCase "should check radio by aria role" $
          withChromiumPage $ \page -> do
            setContent
              page
              "<div role='radio' id='checkbox'>CHECKBOX</div><script>checkbox.addEventListener('click', () => checkbox.setAttribute('aria-checked', 'true'));</script>"
            check (locate page "div")
            assertEqual "aria" "true" =<< evaluateText page "() => window.checkbox.getAttribute('aria-checked')"
      , testCase "should uncheck radio by aria role" $
          withChromiumPage $ \page -> do
            setContent
              page
              "<div role='radio' id='checkbox' aria-checked=\"true\">CHECKBOX</div><script>checkbox.addEventListener('click', () => checkbox.setAttribute('aria-checked', 'false'));</script>"
            uncheck (locate page "div")
            assertEqual "aria" "false" =<< evaluateText page "() => window.checkbox.getAttribute('aria-checked')"
      , testCase "should throw when not a checkbox" $
          withChromiumPage $ \page -> do
            setContent page "<div>Check me</div>"
            assertProtocolContains "Not a checkbox or radio button" (check (locate page "div"))
      , testCase "should throw when not a checkbox 2" $
          withChromiumPage $ \page -> do
            setContent page "<div role=button>Check me</div>"
            assertProtocolContains "Not a checkbox or radio button" (check (locate page "div"))
      , testCase "should check the box inside a button" $
          withChromiumPage $ \page -> do
            setContent page "<div role='button'><input type='checkbox'></div>"
            check (locate page "input")
            assertEqual "eval" True =<< evaluateOnLocatorBool (locate page "input") "input => input.checked"
            assertEqual "isChecked" True =<< isChecked (locate page "input")
      , testCase "trial run should not check" $
          withChromiumPage $ \page -> do
            setContent page "<input id='checkbox' type='checkbox'></input>"
            checkWith (locate page "input") (defaultCheckOptions {checkOptionsTrial = Just True})
            assertEqual "checked" False =<< evaluateBool page "() => window.checkbox.checked"
      , testCase "trial run should not uncheck" $
          withChromiumPage $ \page -> do
            setContent page "<input id='checkbox' type='checkbox' checked></input>"
            uncheckWith (locate page "input") (defaultCheckOptions {checkOptionsTrial = Just True})
            assertEqual "checked" True =<< evaluateBool page "() => window.checkbox.checked"
      , testCase "should check the box using setChecked" $
          withChromiumPage $ \page -> do
            setContent page "<input id='checkbox' type='checkbox'></input>"
            setChecked (locate page "input") True
            assertEqual "checked" True =<< evaluateBool page "() => window.checkbox.checked"
            setChecked (locate page "input") False
            assertEqual "unchecked" False =<< evaluateBool page "() => window.checkbox.checked"
      , testCase "should throw when trying to uncheck radio button" $
          withChromiumPage $ \page -> do
            setContent page "<input type='radio' name='test' checked id='radio'>"
            assertProtocolContains "Cannot uncheck radio button" (uncheck (locate page "#radio"))
    , testCase "should check the label with position" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<input id='checkbox' type='checkbox' style='width: 5px; height: 5px;'><label for='checkbox'><a href=\"about:blank\">I am a long link that goes away so that nothing good will happen if you click on me</a>Click me</label>"
          label <- requireHandle =<< querySelector page (selectorFromText "text=Click me")
          box <- boundingBox label
          case box of
            Nothing ->
              assertFailure "expected a bounding box"
            Just found ->
              checkWith
                (locate page "text=Click me")
                defaultCheckOptions
                  { checkOptionsPosition =
                      Just (Point (CssPixel (cssPixelToScientific (boundingBoxWidth found) - 10)) (cssPixelFromInt 2))
                  }
          assertEqual "checked" True =<< evaluateBool page "() => document.querySelector('input').checked"
      ]
        <> fmap checkAriaRole ariaRoles
        <> fmap uncheckAriaRole ariaRoles
    )

ariaRoles :: [Text]
ariaRoles =
  [ "checkbox"
  , "menuitemcheckbox"
  , "option"
  , "radio"
  , "switch"
  , "menuitemradio"
  , "treeitem"
  ]

checkAriaRole :: Text -> TestTree
checkAriaRole role =
  testCase ("should check the box by aria role " <> unpack role) $
    withChromiumPage $ \page -> do
      setContent
        page
        ( "<div role='"
            <> role
            <> "' id='checkbox'>CHECKBOX</div><script>checkbox.addEventListener('click', () => checkbox.setAttribute('aria-checked', 'true'));</script>"
        )
      check (locate page "div")
      assertEqual "aria" "true" =<< evaluateText page "() => window.checkbox.getAttribute('aria-checked')"

uncheckAriaRole :: Text -> TestTree
uncheckAriaRole role =
  testCase ("should uncheck the box by aria role " <> unpack role) $
    withChromiumPage $ \page -> do
      setContent
        page
        ( "<div role='"
            <> role
            <> "' id='checkbox' aria-checked=\"true\">CHECKBOX</div><script>checkbox.addEventListener('click', () => checkbox.setAttribute('aria-checked', 'false'));</script>"
        )
      uncheck (locate page "div")
      assertEqual "aria" "false" =<< evaluateText page "() => window.checkbox.getAttribute('aria-checked')"

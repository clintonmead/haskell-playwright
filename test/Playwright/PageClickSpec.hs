module Playwright.PageClickSpec
  ( tests
  ) where

import Playwright
  ( ClickOptions (clickOptionsModifiers)
  , Key (Key)
  , KeyboardModifier (KeyboardModifierShift)
  , click
  , clickWith
  , defaultClickOptions
  , defaultGotoOptions
  , evaluate
  , evaluateBool
  , evaluateNumber
  , evaluateText
  , goto
  , keyboardDown
  , keyboardUp
  , setContent
  , urlFromText
  )
import Playwright.Harness
  ( locate
  , withAssetPage
  , withAssets
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
    "page-click"
    [ testCase "should click the button" $
        withAssetPage "input/button.html" $ \page -> do
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click svg" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<svg height=\"100\" width=\"100\"><circle onclick=\"window.__CLICKED=42\" cx=\"50\" cy=\"50\" r=\"40\" stroke=\"black\" stroke-width=\"3\" fill=\"red\" /></svg>"
          click (locate page "circle")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.__CLICKED"
    , testCase "should click the button if window.Node is removed" $
        withAssetPage "input/button.html" $ \page -> do
          _ <- evaluate page "() => { delete window.Node; }"
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click on a span with an inline element inside" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>span::before { content: 'q'; }</style><span onclick='window.CLICKED=42'></span>"
          click (locate page "span")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.CLICKED"
    , testCase "should click the aligned 1x1 div" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the half-aligned 1x1 div" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.5px; margin-top: 11.5px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v1" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.23px; margin-top: 11.65px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v2" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.68px; margin-top: 11.13px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v3" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.68px; margin-top: 11.52px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the unaligned 1x1 div v4" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"margin-left: 20.15px; margin-top: 11.24px; width: 1px; height: 1px;\" onclick=\"window.__clicked = true\"></div>"
          click (locate page "div")
          assertEqual "clicked" True =<< evaluateBool page "() => window.__clicked"
    , testCase "should click the button after navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          click (locate page "button")
          _ <- goto page (urlFromText (prefix <> "/input/button.html")) defaultGotoOptions
          click (locate page "button")
          assertEqual "clicked" "Clicked" =<< evaluateText page "() => window.result"
    , testCase "should click when one of inline box children is outside of viewport" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>i { position: absolute; top: -1000px; }</style><span onclick='window.CLICKED = 42;'><i>woof</i><b>doggo</b></span>"
          click (locate page "span")
          assertEqual "clicked" (42 :: Double) =<< evaluateNumber page "() => window.CLICKED"
    , testCase "should update modifiers correctly" $
        withAssetPage "input/button.html" $ \page -> do
          clickWith (locate page "button") defaultClickOptions {clickOptionsModifiers = Just [KeyboardModifierShift]}
          assertEqual "shift" True =<< evaluateBool page "() => shiftKey"
          clickWith (locate page "button") defaultClickOptions {clickOptionsModifiers = Just []}
          assertEqual "cleared" False =<< evaluateBool page "() => shiftKey"
          keyboardDown page (Key "Shift")
          clickWith (locate page "button") defaultClickOptions {clickOptionsModifiers = Just []}
          assertEqual "override" False =<< evaluateBool page "() => shiftKey"
          click (locate page "button")
          assertEqual "held" True =<< evaluateBool page "() => shiftKey"
          keyboardUp page (Key "Shift")
          click (locate page "button")
          assertEqual "released" False =<< evaluateBool page "() => shiftKey"
    ]

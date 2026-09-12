module Playwright.PageClickReactSpec
  ( tests
  ) where

import Playwright
  ( ClickOptions (clickOptionsTimeout)
  , SpecialJsValue (SpecialUndefined)
  , Timeout
  , armDialog
  , click
  , clickWith
  , defaultClickOptions
  , dismissDialog
  , evaluate
  , evaluateBool
  , evaluateSpecial
  , milliseconds
  , setContent
  )
import Playwright.Harness
  ( assertProtocolContains
  , locate
  , withAssetPage
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , localOption
  , mkTimeout
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  localOption (mkTimeout 30000000) $
    testGroup
      "page-click-react"
      [ testCase "should timeout when click opens alert" $
          withChromiumPage $ \page -> do
            setContent page "<div onclick='window.alert(123)'>Click me</div>"
            takeDialog <- armDialog page
            timeout <- requireTimeout 3000
            assertProtocolContains
              "Timeout 3000ms exceeded"
              ( clickWith
                  (locate page "div")
                  defaultClickOptions {clickOptionsTimeout = Just timeout}
              )
            dialog <- takeDialog
            dismissDialog dialog
      , testCase "should not retarget when element changes on hover" $
          withAssetPage "react.html" $ \page -> do
            _ <-
              evaluate
                page
                "() => { renderComponent(e('div', {}, [e(MyButton, { name: 'button1', renameOnHover: true }), e(MyButton, { name: 'button2' })])); }"
            click (locate page "text=button1")
            assertEqual "button1" True =<< evaluateBool page "() => window.button1"
            special <- evaluateSpecial page "() => window.button2"
            assertEqual "button2" SpecialUndefined special
      , testCase "should not retarget when element is recycled on hover" $
          withAssetPage "react.html" $ \page -> do
            _ <-
              evaluate
                page
                "() => { function shuffle() { renderComponent(e('div', {}, [e(MyButton, { name: 'button2' }), e(MyButton, { name: 'button1' })])); } renderComponent(e('div', {}, [e(MyButton, { name: 'button1', onHover: shuffle }), e(MyButton, { name: 'button2' })])); }"
            click (locate page "text=button1")
            special <- evaluateSpecial page "() => window.button1"
            assertEqual "button1" SpecialUndefined special
            assertEqual "button2" True =<< evaluateBool page "() => window.button2"
      ]

requireTimeout :: Double -> IO Timeout
requireTimeout n =
  case milliseconds n of
    Nothing ->
      assertFailure ("milliseconds " <> show n)
    Just timeout ->
      pure timeout

module Playwright.PageMouseSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( Point (Point)
  , cssPixelFromInt
  , evaluateBool
  , evaluateNumber
  , evaluateText
  , mouseClick
  )
import Playwright.Harness
  ( withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-mouse"
    [ testCase "should click the document" $
        withChromiumPage $ \page -> do
          _ <-
            evaluateBool
              page
              "() => { window.clickPromise = new Promise(resolve => { document.addEventListener('click', event => { resolve({ type: event.type, detail: event.detail, clientX: event.clientX, clientY: event.clientY, isTrusted: event.isTrusted, button: event.button }); }); }); return true; }"
          mouseClick page (Point (cssPixelFromInt 50) (cssPixelFromInt 60))
          eventType <- evaluateText page "() => window.clickPromise.then(event => event.type)"
          clientX <- evaluateNumber page "() => window.clickPromise.then(event => event.clientX)"
          clientY <- evaluateNumber page "() => window.clickPromise.then(event => event.clientY)"
          trusted <- evaluateBool page "() => window.clickPromise.then(event => event.isTrusted)"
          button <- evaluateNumber page "() => window.clickPromise.then(event => event.button)"
          assertEqual "type" ("click" :: Text) eventType
          assertEqual "clientX" (50 :: Double) clientX
          assertEqual "clientY" (60 :: Double) clientY
          assertEqual "trusted" True trusted
          assertEqual "button" (0 :: Double) button
    ]

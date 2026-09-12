module Playwright.JsHandleAsElementSpec
  ( tests
  ) where

import Playwright
  ( asElement
  , elementArgument
  , evaluate
  , evaluateBoolWith
  , evaluateHandle
  , setContent
  )
import Playwright.Harness
  ( requireHandle
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
    "jshandle-as-element"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => document.body"
          _ <- requireHandle =<< asElement handle
          pure ()
    , testCase "should return null for non-elements" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => 2"
          element <- asElement handle
          assertEqual "none" True (case element of Nothing -> True; Just _ -> False)
    , testCase "should return ElementHandle for TextNodes" $
        withChromiumPage $ \page -> do
          setContent page "<div>ee!</div>"
          handle <- evaluateHandle page "() => document.querySelector('div').firstChild"
          node <- requireHandle =<< asElement handle
          assertEqual "TEXT_NODE" True
            =<< evaluateBoolWith page "e => e.nodeType === Node.TEXT_NODE" (elementArgument node)
    , testCase "should work with nullified Node" $
        withChromiumPage $ \page -> do
          setContent page "<section>test</section>"
          _ <- evaluate page "() => { delete Node; }"
          handle <- evaluateHandle page "() => document.querySelector('section')"
          _ <- requireHandle =<< asElement handle
          pure ()
    ]

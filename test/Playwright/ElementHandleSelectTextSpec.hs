module Playwright.ElementHandleSelectTextSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( isNothing
  )
import UnliftIO.Async
  ( poll
  , wait
  , withAsync
  )
import Playwright
  ( evaluateOnElement
  , evaluateText
  , milliseconds
  , querySelector
  , selectText
  , selectTextWith
  , selectorFromText
  )
import Playwright.Harness
  ( assertBool
  , assertProtocolContains
  , requireHandle
  , withAssetPage
  , assertEqual
  , assertFailure
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
    "elementhandle-select-text"
    [ testCase "should select textarea" $
        withAssetPage "input/textarea.html" $ \page -> do
          textarea <- requireHandle =<< querySelector page (selectorFromText "textarea")
          _ <- evaluateOnElement textarea "el => { el.value = 'some value'; return true; }"
          selectText textarea
          assertEqual "selected" "some value" =<< evaluateText page "() => window.getSelection().toString()"
    , testCase "should select input" $
        withAssetPage "input/textarea.html" $ \page -> do
          input <- requireHandle =<< querySelector page (selectorFromText "input")
          _ <- evaluateOnElement input "el => { el.value = 'some value'; return true; }"
          selectText input
          assertEqual "selected" "some value" =<< evaluateText page "() => window.getSelection().toString()"
    , testCase "should select plain div" $
        withAssetPage "input/textarea.html" $ \page -> do
          divHandle <- requireHandle =<< querySelector page (selectorFromText "div.plain")
          selectText divHandle
          assertEqual "selected" "Plain div" =<< evaluateText page "() => window.getSelection().toString()"
    , testCase "should timeout waiting for invisible element" $
        withAssetPage "input/textarea.html" $ \page -> do
          textarea <- requireHandle =<< querySelector page (selectorFromText "textarea")
          _ <- evaluateOnElement textarea "e => { e.style.display = 'none'; return true; }"
          case milliseconds 3000 of
            Nothing ->
              assertFailure "milliseconds 3000"
            Just timeout ->
              assertProtocolContains "Timeout 3000ms exceeded" (selectTextWith textarea timeout)
    , testCase "should wait for visible" $
        withAssetPage "input/textarea.html" $ \page -> do
          textarea <- requireHandle =<< querySelector page (selectorFromText "textarea")
          _ <- evaluateOnElement textarea "e => { e.value = 'some value'; e.style.display = 'none'; return true; }"
          case milliseconds 3000 of
            Nothing ->
              assertFailure "milliseconds 3000"
            Just timeout ->
              withAsync (selectTextWith textarea timeout) $ \pending -> do
                liftIO (threadDelay 1000000)
                finished <- poll pending
                assertBool "still waiting" (isNothing finished)
                _ <- evaluateOnElement textarea "e => { e.style.display = 'block'; return true; }"
                wait pending
          assertEqual "selected" "some value" =<< evaluateText page "() => window.getSelection().toString()"
    ]

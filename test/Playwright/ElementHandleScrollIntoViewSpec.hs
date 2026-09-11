module Playwright.ElementHandleScrollIntoViewSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  )
import Data.Text
  ( pack
  )
import Playwright
  ( evaluate
  , evaluateOnElement
  , evaluateOnElementNumber
  , milliseconds
  , querySelector
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , selectorFromText
  , setContent
  )
import Playwright.Harness
  ( assertProtocolContains
  , requireHandle
  , withAssetPage
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "elementhandle-scroll-into-view"
    [ testCase "should work" $
        withAssetPage "offscreenbuttons.html" $ \page ->
          forM_ [0 :: Int .. 10] $ \i -> do
            button <- requireHandle =<< querySelector page (selectorFromText ("#btn" <> pack (show i)))
            before <-
              evaluateOnElementNumber
                button
                "button => button.getBoundingClientRect().right - window.innerWidth"
            assertEqual "before" (fromIntegral (10 * i) :: Double) before
            scrollIntoViewIfNeeded button
            after <-
              evaluateOnElementNumber
                button
                "button => button.getBoundingClientRect().right - window.innerWidth"
            assertBool "scrolled into view" (after <= 0)
            _ <- evaluate page "() => window.scrollTo(0, 0)"
            pure ()
    , testCase "should throw for detached element" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          _ <- evaluateOnElement handle "div => { div.remove(); return true; }"
          assertProtocolContains "Element is not attached to the DOM" (scrollIntoViewIfNeeded handle)
    , testCase "should work for visibility:hidden element" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"visibility:hidden\">Hello</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          scrollIntoViewIfNeeded handle
    , testCase "should work for zero-sized element" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"height:0\">Hello</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          scrollIntoViewIfNeeded handle
    , testCase "should timeout waiting for visible" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"display:none\">Hello</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          case milliseconds 3000 of
            Nothing ->
              assertFailure "milliseconds 3000"
            Just timeout ->
              assertProtocolContains "Timeout 3000ms exceeded" (scrollIntoViewIfNeededWith handle timeout)
    ]

module Playwright.ElementHandleScrollIntoViewSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  )
import Data.Text
  ( pack
  )
import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( isNothing
  )
import Data.Text
  ( Text
  )
import UnliftIO.Async
  ( poll
  , wait
  , withAsync
  )
import Playwright
  ( ElementHandle
  , Playwright
  , evaluate
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
  , assertBool
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
    , testCase "should wait for display:none to become visible" $
        withChromiumPage $ \page -> do
          setContent page "<div style=\"display:none\">Hello</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          waitWhileHidden handle "div => { div.style.display = 'block'; return true; }"
    , testCase "should wait for nested display:none to become visible" $
        withChromiumPage $ \page -> do
          setContent page "<span style=\"display:none\"><div>Hello</div></span>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          waitWhileHidden handle "div => { div.parentElement.style.display = 'block'; return true; }"
    , testCase "should wait for element to stop moving" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<style>@keyframes move { from { margin-left: 0; } to { margin-left: 200px; } } div.animated { animation: 2s linear 0s infinite alternate move; }</style><div class=animated>moving</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "div")
          waitWhileHidden handle "div => { div.classList.remove('animated'); return true; }"
    ]

-- | Scrolls the element into view, checks that the scroll is still pending
-- after a second, then runs the unblocking script and waits for the scroll.
waitWhileHidden :: ElementHandle -> Text -> Playwright ()
waitWhileHidden handle unblock =
  withAsync (scrollIntoViewIfNeeded handle) $ \pending -> do
    liftIO (threadDelay 1000000)
    finished <- poll pending
    assertBool "still waiting" (isNothing finished)
    _ <- evaluateOnElement handle unblock
    wait pending

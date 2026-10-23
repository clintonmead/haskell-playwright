module Playwright.PageLeaksSpec
  ( tests
  ) where

import Control.Monad
  ( forM_
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as T
import Playwright
  ( Locator
  , Playwright
  , Page
  , WaitForState (WaitForVisible)
  , click
  , evaluate
  , evaluateNumber
  , fill
  , isVisible
  , lastLocator
  , locator
  , requestGC
  , selectorFromText
  , setContent
  , waitFor
  )
import Playwright.Harness
  ( assertBool
  , assertFailure
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Concurrent
  ( threadDelay
  )

-- Upstream: tests/page/page-leaks.spec.ts. Only the main-world WeakRef counts
-- are checked. The upstream leakedJSHandles assertion is skipped: it reads
-- the driver's test-mode leak registry, which a client cannot reach. The
-- utility world is not reachable from the client either, so it is not counted.
-- Each case creates 25 dynamic elements, keeps the static ones, and expects
-- between 2 and 24 WeakRefs to survive a garbage collection.

tests :: TestTree
tests =
  testGroup
    "page-leaks"
    [ testCase "click should not leak" $
        withChromiumPage $ \page -> do
          setContent page staticButtons
          forM_ [0 .. 24 :: Int] $ \i -> do
            appendElement page "button" "buttons" i
            click (lastLocator (buttonLocator page))
          weakRefObjects page "button"
          clearContainer page "buttons"
          checkWeakRefs page 2 25
    , testCase "fill should not leak" $
        withChromiumPage $ \page -> do
          setContent page staticInputs
          forM_ [0 .. 24 :: Int] $ \i -> do
            appendElement page "input" "inputs" i
            fill (lastLocator (inputLocator page)) ("input " <> T.pack (show i))
          weakRefObjects page "input"
          clearContainer page "inputs"
          checkWeakRefs page 2 25
    , testCase "expect should not leak" $
        withChromiumPage $ \page -> do
          setContent page staticButtons
          weakRefObjects page "button"
          forM_ [0 .. 24 :: Int] $ \i -> do
            appendElement page "button" "buttons" i
            assertBool "visible" =<< isVisible (lastLocator (buttonLocator page))
          weakRefObjects page "button"
          clearContainer page "buttons"
          checkWeakRefs page 2 25
    , testCase "waitFor should not leak" $
        withChromiumPage $ \page -> do
          setContent page staticButtons
          forM_ [0 .. 24 :: Int] $ \i -> do
            appendElement page "button" "buttons" i
            waitFor (lastLocator (buttonLocator page)) WaitForVisible
          weakRefObjects page "button"
          clearContainer page "buttons"
          checkWeakRefs page 2 25
    ]

staticButtons :: Text
staticButtons =
  "<button>static button 1</button>\n<button>static button 2</button>\n<div id=\"buttons\"></div>"

staticInputs :: Text
staticInputs =
  "<input value=\"static input 1\">\n<input value=\"static input 2\">\n<div id=\"inputs\"></div>"

buttonLocator :: Page -> Locator
buttonLocator page = locator page (selectorFromText "#buttons > button")

inputLocator :: Page -> Locator
inputLocator page = locator page (selectorFromText "#inputs > input")

-- Appends one element of the given tag to the container matching its name.
appendElement :: Page -> Text -> Text -> Int -> Playwright ()
appendElement page tag container i = do
  let label = "dynamic " <> T.pack (show i)
  _ <-
    evaluate
      page
      ( "() => { const element = document.createElement('"
          <> tag
          <> "'); element.textContent = '"
          <> label
          <> "'; document.getElementById('"
          <> container
          <> "').appendChild(element); }"
      )
  pure ()

-- Records a WeakRef to every element matching the tag in the main world.
weakRefObjects :: Page -> Text -> Playwright ()
weakRefObjects page tag = do
  _ <-
    evaluate
      page
      ( "() => { const elements = document.querySelectorAll('"
          <> tag
          <> "'); globalThis.weakRefs = globalThis.weakRefs || []; for (const element of elements) globalThis.weakRefs.push(new WeakRef(element)); }"
      )
  pure ()

clearContainer :: Page -> Text -> Playwright ()
clearContainer page container = do
  _ <-
    evaluate
      page
      ("() => { document.getElementById('" <> container <> "').textContent = ''; }")
  pure ()

checkWeakRefs :: Page -> Int -> Int -> Playwright ()
checkWeakRefs page from to = retryWeakRefs page from to (20 :: Int)

-- Collection is not immediate, so the count is retried for a short while.
retryWeakRefs :: Page -> Int -> Int -> Int -> Playwright ()
retryWeakRefs page from to attempts = do
  requestGC page
  count <-
    evaluateNumber
      page
      "() => globalThis.weakRefs.filter(r => !!r.deref()).length"
  if count >= fromIntegral from && count < fromIntegral to
    then pure ()
    else
      if attempts <= 0
        then assertFailure ("weak ref count " <> show count <> " not in [" <> show from <> ", " <> show to <> ")")
        else do
          threadDelay 100000
          retryWeakRefs page from to (attempts - 1)

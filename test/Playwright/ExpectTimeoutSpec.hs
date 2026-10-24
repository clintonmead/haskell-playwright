-- | Ports of upstream tests/page/expect-timeout.spec.ts.
--
-- Failure cases assert that the matcher throws. The upstream assertions on
-- the rendered failure message (the header lines, the call log and the
-- locator handler log lines) are not ported, because the Haskell client does
-- not expose the driver's formatted message. The @AbortSignal@ cases are not
-- ported, because 'ExpectOptions' has no signal option. The upstream
-- @about:blank@ navigation target is replaced with a @goto@ to the same
-- blank page, since these tests only need a navigation to happen.
module Playwright.ExpectTimeoutSpec
  ( tests
  ) where

import Control.Monad
  ( void
  )
import Playwright
  ( ExpectOptions (expectTimeout)
  , ExpectedText (ExpectedString)
  , WaitForState (WaitForAttached)
  , addLocatorHandler
  , defaultExpectOptions
  , defaultGotoOptions
  , evaluate
  , expectHaveText
  , expectLocatorWith
  , goto
  , Playwright
  , selectorFromText
  , setContent
  , urlFromText
  , waitForSelectorHandleWith
  )
import Playwright.Error
  ( PlaywrightError
  )
import Playwright.Harness
  ( assertBool
  , assertProtocolContains
  , locate
  , requireTimeout
  , withChromiumPage
  )
import Playwright.Timeout
  ( milliseconds
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( concurrently_
  )
import UnliftIO.Exception
  ( try
  )
tests :: TestTree
tests =
  testGroup
    "expect-timeout"
    [ testCase "should print element not found" $
        withChromiumPage $ \page -> do
          setContent page "<div id=node>Text content</div>"
          assertFails $
            expectHaveText (timeoutOpts 1000) (locate page "no-such-thing") (ExpectedString "hey")
    , testCase "should print timed out error message when value does not match" $
        withChromiumPage $ \page -> do
          setContent page "<div id=node>Text content</div>"
          assertFails $
            expectHaveText (timeoutOpts 1000) (locate page "div") (ExpectedString "hey")
    , testCase "should print timed out error message with impossible timeout" $
        withChromiumPage $ \page -> do
          setContent page "<div id=node>Text content</div>"
          assertFails $
            expectHaveText (timeoutOpts 1) (locate page "no-such-thing") (ExpectedString "hey")
    , testCase "should print timed out error message when value does not match with impossible timeout" $
        withChromiumPage $ \page -> do
          setContent page "<div id=node>Text content</div>"
          assertFails $
            expectHaveText (timeoutOpts 1) (locate page "div") (ExpectedString "hey")
    , testCase "should have timeout error name" $
        withChromiumPage $ \page -> do
          timeout <- requireTimeout 1
          assertProtocolContains "Timeout 1ms exceeded" $
            void (waitForSelectorHandleWith page (selectorFromText "#not-found") WaitForAttached timeout)
    , testCase "should not throw when navigating during one-shot check" $
        withChromiumPage $ \page -> do
          setContent page "<div>hello</div>"
          concurrently_
            (expectHaveText defaultExpectOptions (locate page "div") (ExpectedString "bye"))
            ( do
                _ <- goto page (urlFromText "about:blank") defaultGotoOptions
                setContent page "<div>bye</div>"
            )
    , testCase "should not throw when navigating during first locator handler check" $
        withChromiumPage $ \page -> do
          addLocatorHandler page (locate page "span") (\_ -> pure ())
          setContent page "<div>hello</div>"
          concurrently_
            (expectHaveText defaultExpectOptions (locate page "div") (ExpectedString "bye"))
            ( do
                _ <- goto page (urlFromText "about:blank") defaultGotoOptions
                setContent page "<div>bye</div>"
            )
    , testCase "should timeout during first locator handler check" $
        withChromiumPage $ \page -> do
          addLocatorHandler page (locate page "div") (\_ -> pure ())
          setContent page "<div>hello</div><span>bye</span>"
          assertFails $
            expectHaveText (timeoutOpts 3000) (locate page "span") (ExpectedString "bye")
    , testCase "should not miss element that appears between retries before the deadline" $
        withChromiumPage $ \page -> do
          setContent page "<div id=\"target\" style=\"display:none\">content</div>"
          void $
            evaluate
              page
              "() => { setTimeout(() => { document.getElementById('target').style.display = 'block'; }, 1500); }"
          expectLocatorWith (timeoutOpts 1800) (locate page "#target") "to.be.visible"
    ]

timeoutOpts :: Double -> ExpectOptions
timeoutOpts ms = defaultExpectOptions {expectTimeout = milliseconds ms}

assertFails :: Playwright () -> Playwright ()
assertFails action = do
  outcome <- try action :: Playwright (Either PlaywrightError ())
  case outcome of
    Left _ -> pure ()
    Right _ -> assertBool "expectation should have failed" False

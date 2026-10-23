module Playwright.LocatorWaitForFunctionSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  , pack
  )
import Playwright
  ( Playwright
  , elementArgument
  , evaluate
  , querySelector
  , selectorFromText
  , setContent
  , waitForFunctionOnLocator
  , waitForFunctionOnLocatorWithArg
  )
import Playwright.Harness
  ( assertBool
  , assertFailure
  , locate
  , requireHandle
  , withChromiumPage
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO
  ( try
  )
import UnliftIO.Exception
  ( SomeException
  )

tests :: TestTree
tests =
  testGroup
    "locator-wait-for-function"
    [ testCase "should wait for an attribute to appear" $
        withChromiumPage $ \page -> do
          setContent page "<button id=toggle>Menu</button>"
          _ <-
            evaluate
              page
              "() => setTimeout(() => document.querySelector('#toggle').setAttribute('aria-expanded', 'true'), 1000)"
          waitForFunctionOnLocator (locate page "#toggle") "element => element.hasAttribute('aria-expanded')"
    , testCase "should return immediately when already truthy" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target>yes</div>"
          waitForFunctionOnLocator (locate page "#target") "element => element.textContent === 'yes'"
    , testCase "should accept ElementHandle arguments" $
        withChromiumPage $ \page -> do
          setContent page "<div id=a></div><div id=b>value</div>"
          handle <- requireHandle =<< querySelector page (selectorFromText "#b")
          waitForFunctionOnLocatorWithArg
            (locate page "#a")
            "(element, other) => other.textContent === 'value'"
            (elementArgument handle)
    , testCase "should accept string expression" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target>yes</div>"
          waitForFunctionOnLocator (locate page "#target") "element => element.textContent === 'yes'"
    , testCase "should resolve a promise returned by the predicate" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target>yes</div>"
          waitForFunctionOnLocator (locate page "#target") "async element => element.textContent === 'yes'"
    , testCase "should wait for element to appear and survive rerender" $
        withChromiumPage $ \page -> do
          setContent page "<span>nothing here</span>"
          _ <-
            evaluate
              page
              "() => { let count = 0; let prev = null; const tick = () => { ++count; const next = document.createElement('div'); next.id = 'target'; next.textContent = String(count); if (prev) prev.remove(); document.body.appendChild(next); prev = next; if (count < 3) setTimeout(tick, 500); }; setTimeout(tick, 500); }"
          waitForFunctionOnLocator (locate page "#target") "element => element.textContent === '3'"
    , testCase "should throw when predicate throws" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target>no</div>"
          expectFailureContaining
            "oh my"
            (waitForFunctionOnLocator (locate page "#target") "() => { throw new Error('oh my'); }")
    , testCase "should throw on strict mode violation" $
        withChromiumPage $ \page -> do
          setContent page "<div class=x>1</div><div class=x>2</div>"
          expectFailureContaining
            "strict mode violation"
            (waitForFunctionOnLocator (locate page "div.x") "() => true")
    ]

-- Runs the action and checks that it throws an error whose message contains the given text.
expectFailureContaining :: Text -> Playwright () -> Playwright ()
expectFailureContaining needle action = do
  result <- try action :: Playwright (Either SomeException ())
  case result of
    Left err ->
      assertBool "error message" (needle `isInfixOf` pack (show err))
    Right () ->
      assertFailure "expected an error"

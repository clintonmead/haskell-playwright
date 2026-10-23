module Playwright.Expect
  ( ExpectOptions (..)
  , defaultExpectOptions
  , expectLocator
  , expectLocatorWith
  , expectLocatorCount
  , sendExpect
  , expectHaveText
  , expectHaveTextArray
  , expectContainText
  , expectContainTextArray
  , expectHaveValue
  , expectHaveValues
  ) where

import Data.Aeson
  ( Value
  )
import Data.Text
  ( Text
  )
import Playwright.ExpectedText
  ( ExpectedText
  , expectedTextValue
  )
import Playwright.Frame
  ( Frame
  , frameSend_
  , frameSendTimed_
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Protocol
  ( ExpectedTextValue
  , FrameExpectParams
  , frameExpectParamsExpectedNumber
  , frameExpectParamsExpectedText
  , frameExpectParamsSelector
  , frameExpectParamsUseInnerText
  , mkFrameExpectParams
  )
import Playwright.Timeout
  ( Timeout
  )

-- | Options shared by every expect matcher.
--
-- 'expectIsNot' inverts the matcher, and 'expectTimeout' bounds the
-- driver-side polling. 'Nothing' leaves the timeout to the driver default.
--
-- 'expectIgnoreCase' and 'expectUseInnerText' apply only to the text
-- matchers. 'Nothing' leaves them to the driver default.
data ExpectOptions = ExpectOptions
  { expectIsNot :: Bool
  , expectTimeout :: Maybe Timeout
  , expectIgnoreCase :: Maybe Bool
  , expectUseInnerText :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultExpectOptions :: ExpectOptions
defaultExpectOptions =
  ExpectOptions
    { expectIsNot = False
    , expectTimeout = Nothing
    , expectIgnoreCase = Nothing
    , expectUseInnerText = Nothing
    }

-- | Run a boolean locator matcher such as @to.be.visible@ through the
-- driver's server-side polling. The call throws the driver's error when the
-- assertion does not hold within the timeout.
expectLocator :: Locator -> Text -> Playwright ()
expectLocator = expectLocatorWith defaultExpectOptions

expectLocatorWith :: ExpectOptions -> Locator -> Text -> Playwright ()
expectLocatorWith options locator expression =
  sendExpect
    options
    (locatorFrame locator)
    (mkFrameExpectParams expression (expectIsNot options))
      { frameExpectParamsSelector = Just (locatorSelector locator)
      }

-- | @to.have.count@ with a numeric expectation.
expectLocatorCount :: ExpectOptions -> Locator -> Int -> Playwright ()
expectLocatorCount options locator count =
  sendExpect
    options
    (locatorFrame locator)
    (mkFrameExpectParams "to.have.count" (expectIsNot options))
      { frameExpectParamsSelector = Just (locatorSelector locator)
      , frameExpectParamsExpectedNumber = Just (fromIntegral count)
      }

-- | The element text equals the expectation, with whitespace normalized.
expectHaveText :: ExpectOptions -> Locator -> ExpectedText -> Playwright ()
expectHaveText options locator expected =
  expectTextWith options locator "to.have.text" $
    [expectedTextValue False True (expectIgnoreCase options) expected]

-- | The element list texts equal the expectations, one per element.
expectHaveTextArray :: ExpectOptions -> Locator -> [ExpectedText] -> Playwright ()
expectHaveTextArray options locator expected =
  expectTextWith options locator "to.have.text.array" $
    map (expectedTextValue False True (expectIgnoreCase options)) expected

-- | The element text contains the expectation as a substring.
expectContainText :: ExpectOptions -> Locator -> ExpectedText -> Playwright ()
expectContainText options locator expected =
  expectTextWith options locator "to.have.text" $
    [expectedTextValue True True (expectIgnoreCase options) expected]

-- | Each expectation is a substring of the matching element text.
expectContainTextArray :: ExpectOptions -> Locator -> [ExpectedText] -> Playwright ()
expectContainTextArray options locator expected =
  expectTextWith options locator "to.contain.text.array" $
    map (expectedTextValue True True (expectIgnoreCase options)) expected

-- | The input value equals the expectation.
expectHaveValue :: ExpectOptions -> Locator -> ExpectedText -> Playwright ()
expectHaveValue options locator expected =
  expectTextWith (valueOptions options) locator "to.have.value" $
    [expectedTextValue False False Nothing expected]

-- | The select element's selected values equal the expectations.
expectHaveValues :: ExpectOptions -> Locator -> [ExpectedText] -> Playwright ()
expectHaveValues options locator expected =
  expectTextWith (valueOptions options) locator "to.have.values" $
    map (expectedTextValue False False Nothing) expected

-- | Value matchers ignore the text-only options, as upstream does.
valueOptions :: ExpectOptions -> ExpectOptions
valueOptions options =
  options
    { expectIgnoreCase = Nothing
    , expectUseInnerText = Nothing
    }

expectTextWith :: ExpectOptions -> Locator -> Text -> [ExpectedTextValue] -> Playwright ()
expectTextWith options locator expression values =
  sendExpect
    options
    (locatorFrame locator)
    (mkFrameExpectParams expression (expectIsNot options))
      { frameExpectParamsSelector = Just (locatorSelector locator)
      , frameExpectParamsExpectedText = Just values
      , frameExpectParamsUseInnerText = expectUseInnerText options
      }

-- | Send a raw @expect@ call to the driver, honouring the optional timeout.
sendExpect :: ExpectOptions -> Frame -> FrameExpectParams -> Playwright ()
sendExpect options frame params =
  case expectTimeout options of
    Nothing ->
      frameSend_ @Value frame "expect" params
    Just timeout ->
      frameSendTimed_ @Value frame "expect" timeout params

module Playwright.ExpectText
  ( TextMatchOptions (..)
  , defaultTextMatchOptions
  , expectLocatorText
  , expectLocatorTextWith
  , expectLocatorRole
  ) where

import Data.Text
  ( Text
  )
import Playwright.AriaRole
  ( AriaRole (ariaRoleToText)
  )
import Playwright.Expect
  ( ExpectOptions (expectIsNot)
  , defaultExpectOptions
  , sendExpect
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Protocol
  ( FrameExpectParams (frameExpectParamsExpectedText, frameExpectParamsSelector)
  , mkExpectedTextValue
  , mkFrameExpectParams
  )
import Playwright.Protocol qualified as Protocol
import Playwright.TextExpectation
  ( TextExpectation (ExpectRegex, ExpectString)
  )
import Playwright.TextProperty
  ( TextProperty
  , textPropertyExpression
  )

-- | Options for text matchers such as @toHaveAccessibleName@.
--
-- 'textMatchIgnoreCase' compares case-insensitively. Whitespace is always
-- normalized, as upstream does for these matchers.
newtype TextMatchOptions = TextMatchOptions
  { textMatchIgnoreCase :: Bool
  }
  deriving stock (Eq, Show)

defaultTextMatchOptions :: TextMatchOptions
defaultTextMatchOptions =
  TextMatchOptions
    { textMatchIgnoreCase = False
    }

-- | Match an accessible text property of a locator with default options.
expectLocatorText :: Locator -> TextProperty -> TextExpectation -> Playwright ()
expectLocatorText = expectLocatorTextWith defaultExpectOptions defaultTextMatchOptions

-- | Match an accessible text property of a locator. 'expectIsNot' inverts the
-- matcher and 'expectTimeout' bounds the driver-side polling.
expectLocatorTextWith ::
  ExpectOptions ->
  TextMatchOptions ->
  Locator ->
  TextProperty ->
  TextExpectation ->
  Playwright ()
expectLocatorTextWith options matchOptions locator property expectation =
  sendTextExpect
    options
    locator
    (textPropertyExpression property)
    (textExpectedValue matchOptions expectation)

-- | @toHaveRole@: the element's ARIA role must equal the given role.
expectLocatorRole :: ExpectOptions -> Locator -> AriaRole -> Playwright ()
expectLocatorRole options locator role =
  sendTextExpect
    options
    locator
    "to.have.role"
    (textExpectedValue defaultTextMatchOptions (ExpectString (ariaRoleToText role)))

sendTextExpect :: ExpectOptions -> Locator -> Text -> Protocol.ExpectedTextValue -> Playwright ()
sendTextExpect options locator expression expected =
  sendExpect
    options
    (locatorFrame locator)
    (mkFrameExpectParams expression (expectIsNot options))
      { frameExpectParamsSelector = Just (locatorSelector locator)
      , frameExpectParamsExpectedText = Just [expected]
      }

textExpectedValue :: TextMatchOptions -> TextExpectation -> Protocol.ExpectedTextValue
textExpectedValue matchOptions expectation =
  case expectation of
    ExpectString text ->
      base
        { Protocol.expectedTextValueString = Just text
        }
    ExpectRegex source flags ->
      base
        { Protocol.expectedTextValueRegexSource = Just source
        , Protocol.expectedTextValueRegexFlags = Just flags
        }
  where
    base =
      mkExpectedTextValue
        { Protocol.expectedTextValueIgnoreCase =
            if textMatchIgnoreCase matchOptions then Just True else Nothing
        , Protocol.expectedTextValueNormalizeWhiteSpace = Just True
        }

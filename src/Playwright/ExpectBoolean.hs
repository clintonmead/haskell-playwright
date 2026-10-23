module Playwright.ExpectBoolean
  ( CheckedOptions (..)
  , defaultCheckedOptions
  , expectLocatorChecked
  , expectLocatorEditable
  , expectLocatorEnabled
  ) where

import Data.Maybe
  ( catMaybes
  )
import Data.Text
  ( Text
  )
import Playwright.Expect
  ( ExpectOptions (expectIsNot)
  , expectLocatorWith
  , sendExpect
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Protocol
  ( frameExpectParamsExpectedValue
  , frameExpectParamsSelector
  , mkFrameExpectParams
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , objectArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadBool)
  , SerializedValue (SerializedNode)
  )

-- | Options for @toBeChecked@.
--
-- 'checkedChecked' selects between checked ('Just True', the default when
-- unset) and unchecked ('Just False'). 'checkedIndeterminate' asserts the
-- indeterminate state. 'Nothing' leaves a field out of the expectation.
data CheckedOptions = CheckedOptions
  { checkedChecked :: Maybe Bool
  , checkedIndeterminate :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultCheckedOptions :: CheckedOptions
defaultCheckedOptions =
  CheckedOptions
    { checkedChecked = Nothing
    , checkedIndeterminate = Nothing
    }

-- | @to.be.checked@, with the optional @checked@ and @indeterminate@
-- expectations sent as the driver's @expectedValue@.
expectLocatorChecked :: ExpectOptions -> CheckedOptions -> Locator -> Playwright ()
expectLocatorChecked options checkedOptions locator =
  sendExpect
    options
    (locatorFrame locator)
    (mkFrameExpectParams "to.be.checked" (expectIsNot options))
      { frameExpectParamsSelector = Just (locatorSelector locator)
      , frameExpectParamsExpectedValue = checkedExpectedValue checkedOptions
      }

-- | Upstream always sends the expectation object, even when empty, and the
-- driver does not evaluate @to.be.checked@ without it.
checkedExpectedValue :: CheckedOptions -> Maybe SerializedArgument
checkedExpectedValue options =
  Just (objectArgument fields)
  where
    fields :: [(Text, SerializedValue)]
    fields =
      catMaybes
        [ fmap (\flag -> ("checked", boolValue flag)) (checkedChecked options)
        , fmap (\flag -> ("indeterminate", boolValue flag)) (checkedIndeterminate options)
        ]

    boolValue :: Bool -> SerializedValue
    boolValue flag = SerializedNode (PayloadBool flag) Nothing

-- | @to.be.editable@ when the flag is 'True', otherwise @to.be.readonly@.
expectLocatorEditable :: ExpectOptions -> Bool -> Locator -> Playwright ()
expectLocatorEditable options editable locator =
  expectLocatorWith
    options
    locator
    (if editable then "to.be.editable" else "to.be.readonly")

-- | @to.be.enabled@ when the flag is 'True', otherwise @to.be.disabled@.
expectLocatorEnabled :: ExpectOptions -> Bool -> Locator -> Playwright ()
expectLocatorEnabled options enabled locator =
  expectLocatorWith
    options
    locator
    (if enabled then "to.be.enabled" else "to.be.disabled")

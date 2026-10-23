module Playwright.ExpectedText
  ( ExpectedText (..)
  , expectedTextValue
  ) where

import Data.Text
  ( Text
  )
import Playwright.Protocol
  ( ExpectedTextValue
      ( ExpectedTextValue
      , expectedTextValueIgnoreCase
      , expectedTextValueMatchSubstring
      , expectedTextValueNormalizeWhiteSpace
      , expectedTextValueRegexFlags
      , expectedTextValueRegexSource
      , expectedTextValueString
      )
  )

-- | One expected text: either an exact string, or a regular expression given
-- by its source and flags.
data ExpectedText
  = ExpectedString Text
  | ExpectedRegex Text Text
  deriving stock (Eq, Show)

-- | Build the wire value the driver matches against.
--
-- 'matchSubstring' and 'normalizeWhiteSpace' are sent only when set, so
-- the driver default applies otherwise. 'ignoreCase' is passed through as
-- given.
expectedTextValue :: Bool -> Bool -> Maybe Bool -> ExpectedText -> ExpectedTextValue
expectedTextValue matchSubstring normalizeWhiteSpace ignoreCase expected =
  ExpectedTextValue
    { expectedTextValueIgnoreCase = ignoreCase
    , expectedTextValueMatchSubstring = flag matchSubstring
    , expectedTextValueNormalizeWhiteSpace = flag normalizeWhiteSpace
    , expectedTextValueRegexFlags = regexFlags
    , expectedTextValueRegexSource = regexSource
    , expectedTextValueString = string
    }
  where
    flag True = Just True
    flag False = Nothing
    (string, regexSource, regexFlags) =
      case expected of
        ExpectedString text -> (Just text, Nothing, Nothing)
        ExpectedRegex source flags -> (Nothing, Just source, Just flags)

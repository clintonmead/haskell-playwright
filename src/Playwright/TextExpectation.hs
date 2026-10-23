module Playwright.TextExpectation
  ( TextExpectation (..)
  ) where

import Data.Text
  ( Text
  )

-- | The text a matcher expects: a literal string, or a regular expression
-- given as its source and its flags (for example @"i"@).
data TextExpectation
  = ExpectString Text
  | ExpectRegex Text Text
  deriving stock (Eq, Show)

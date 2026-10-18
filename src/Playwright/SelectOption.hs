module Playwright.SelectOption
  ( SelectAttributes (..)
  , SelectOption (..)
  ) where

import Data.Text
  ( Text
  )

-- | An option matched by any combination of its value, label and index. A
-- matching option must satisfy every attribute that is set.
data SelectAttributes = SelectAttributes
  { selectAttributesValue :: Maybe Text
  , selectAttributesLabel :: Maybe Text
  , selectAttributesIndex :: Maybe Int
  }
  deriving stock (Eq, Show)

data SelectOption
  = SelectValueOrLabel Text
  | SelectValue Text
  | SelectLabel Text
  | SelectIndex Int
  | SelectMatching SelectAttributes
  deriving stock (Eq, Show)

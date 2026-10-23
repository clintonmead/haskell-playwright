module Playwright.TextProperty
  ( TextProperty (..)
  , textPropertyExpression
  ) where

import Data.Text
  ( Text
  )

-- | The element text that a text matcher reads.
data TextProperty
  = AccessibleName
  | AccessibleDescription
  | AccessibleErrorMessage
  deriving stock (Eq, Show)

-- | The driver expression that reads the given property.
textPropertyExpression :: TextProperty -> Text
textPropertyExpression property =
  case property of
    AccessibleName -> "to.have.accessible.name"
    AccessibleDescription -> "to.have.accessible.description"
    AccessibleErrorMessage -> "to.have.accessible.error.message"

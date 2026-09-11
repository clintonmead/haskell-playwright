module Playwright.ScriptTag
  ( ScriptTag (..)
  , emptyScriptTag
  ) where

import Data.Text
  ( Text
  )
import Playwright.Url
  ( Url
  )

data ScriptTag = ScriptTag
  { scriptTagContent :: Maybe Text
  , scriptTagType :: Maybe Text
  , scriptTagUrl :: Maybe Url
  }
  deriving stock (Eq, Show)

emptyScriptTag :: ScriptTag
emptyScriptTag =
  ScriptTag
    { scriptTagContent = Nothing
    , scriptTagType = Nothing
    , scriptTagUrl = Nothing
    }

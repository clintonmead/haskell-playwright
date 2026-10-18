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

-- | Read from scriptTagPath when set; it replaces scriptTagContent, as in the
-- JS client.
data ScriptTag = ScriptTag
  { scriptTagContent :: Maybe Text
  , scriptTagPath :: Maybe FilePath
  , scriptTagType :: Maybe Text
  , scriptTagUrl :: Maybe Url
  }
  deriving stock (Eq, Show)

emptyScriptTag :: ScriptTag
emptyScriptTag =
  ScriptTag
    { scriptTagContent = Nothing
    , scriptTagPath = Nothing
    , scriptTagType = Nothing
    , scriptTagUrl = Nothing
    }

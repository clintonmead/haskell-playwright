module Playwright.GetBy
  ( getByLabelSelector
  , getByRoleSelector
  , getByTestIdSelector
  , getByTextSelector
  ) where

import Data.Aeson
  ( encode
  )
import Data.ByteString.Lazy
  ( toStrict
  )
import Data.Maybe
  ( catMaybes
  )
import Data.Text
  ( Text
  , pack
  , replace
  )
import Data.Text.Encoding
  ( decodeUtf8
  )
import Playwright.AriaRole
  ( AriaRole (ariaRoleToText)
  )
import Playwright.Exactness
  ( Exactness (Exact, Inexact)
  )
import Playwright.GetByRoleOptions
  ( GetByRoleOptions (getByRoleChecked, getByRoleDisabled, getByRoleExact, getByRoleExpanded, getByRoleIncludeHidden, getByRoleLevel, getByRoleName, getByRolePressed, getByRoleSelected)
  )
import Playwright.Selector
  ( Selector
  , selectorFromText
  )

getByTextSelector :: Text -> Exactness -> Selector
getByTextSelector text exactness =
  selectorFromText ("internal:text=" <> escapeForTextSelector text exactness)

getByLabelSelector :: Text -> Exactness -> Selector
getByLabelSelector text exactness =
  selectorFromText ("internal:label=" <> escapeForTextSelector text exactness)

getByTestIdSelector :: Text -> Selector
getByTestIdSelector testId =
  selectorFromText ("internal:testid=[data-testid=" <> escapeForAttributeSelector testId Exact <> "]")

getByRoleSelector :: AriaRole -> GetByRoleOptions -> Selector
getByRoleSelector role opts =
  selectorFromText ("internal:role=" <> ariaRoleToText role <> mconcat attrs)
  where
    attrs :: [Text]
    attrs =
      catMaybes
        [ boolAttr "checked" (getByRoleChecked opts)
        , boolAttr "disabled" (getByRoleDisabled opts)
        , boolAttr "selected" (getByRoleSelected opts)
        , boolAttr "expanded" (getByRoleExpanded opts)
        , boolAttr "include-hidden" (getByRoleIncludeHidden opts)
        , fmap (\level -> "[level=" <> pack (show level) <> "]") (getByRoleLevel opts)
        , fmap (\name -> "[name=" <> escapeForAttributeSelector name (getByRoleExact opts) <> "]") (getByRoleName opts)
        , boolAttr "pressed" (getByRolePressed opts)
        ]

boolAttr :: Text -> Maybe Bool -> Maybe Text
boolAttr name value =
  fmap (\flag -> "[" <> name <> "=" <> boolText flag <> "]") value

boolText :: Bool -> Text
boolText flag =
  if flag
    then "true"
    else "false"

escapeForTextSelector :: Text -> Exactness -> Text
escapeForTextSelector text exactness =
  decodeUtf8 (toStrict (encode text)) <> exactnessSuffix exactness

escapeForAttributeSelector :: Text -> Exactness -> Text
escapeForAttributeSelector value exactness =
  "\""
    <> replace "\"" "\\\"" (replace "\\" "\\\\" value)
    <> "\""
    <> exactnessSuffix exactness

exactnessSuffix :: Exactness -> Text
exactnessSuffix exactness =
  case exactness of
    Exact ->
      "s"
    Inexact ->
      "i"

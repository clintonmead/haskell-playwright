module Playwright.GetBy
  ( getByAltTextSelector
  , getByAltTextSelectorRegex
  , getByLabelSelector
  , getByPlaceholderSelector
  , getByPlaceholderSelectorRegex
  , getByRoleSelector
  , getByTestIdSelector
  , getByTestIdSelectorRegex
  , getByTextSelector
  , getByTitleSelector
  , getByTitleSelectorRegex
  , hasNotTextSelector
  , hasTextRegexSelector
  , hasTextSelector
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
  ( GetByRoleOptions (getByRoleChecked, getByRoleDisabled, getByRoleExact, getByRoleExpanded, getByRoleIncludeHidden, getByRoleLevel, getByRoleName, getByRoleNameRegex, getByRolePressed, getByRoleSelected)
  )
import Playwright.Selector
  ( Selector
  , selectorFromText
  )

getByTextSelector :: Text -> Exactness -> Selector
getByTextSelector text exactness =
  selectorFromText ("internal:text=" <> escapeForTextSelector text exactness)

hasTextSelector :: Text -> Exactness -> Selector
hasTextSelector text exactness =
  selectorFromText ("internal:has-text=" <> escapeForTextSelector text exactness)

hasNotTextSelector :: Text -> Exactness -> Selector
hasNotTextSelector text exactness =
  selectorFromText ("internal:has-not-text=" <> escapeForTextSelector text exactness)

hasTextRegexSelector :: Text -> Selector
hasTextRegexSelector source =
  selectorFromText ("internal:has-text=" <> source)

getByLabelSelector :: Text -> Exactness -> Selector
getByLabelSelector text exactness =
  selectorFromText ("internal:label=" <> escapeForTextSelector text exactness)

getByAltTextSelector :: Text -> Exactness -> Selector
getByAltTextSelector =
  getByAttributeSelector "alt"

getByAltTextSelectorRegex :: Text -> Selector
getByAltTextSelectorRegex =
  getByAttributeSelectorRegex "alt"

getByPlaceholderSelector :: Text -> Exactness -> Selector
getByPlaceholderSelector =
  getByAttributeSelector "placeholder"

getByPlaceholderSelectorRegex :: Text -> Selector
getByPlaceholderSelectorRegex =
  getByAttributeSelectorRegex "placeholder"

getByTitleSelector :: Text -> Exactness -> Selector
getByTitleSelector =
  getByAttributeSelector "title"

getByTitleSelectorRegex :: Text -> Selector
getByTitleSelectorRegex =
  getByAttributeSelectorRegex "title"

getByAttributeSelector :: Text -> Text -> Exactness -> Selector
getByAttributeSelector attr text exactness =
  selectorFromText ("internal:attr=[" <> attr <> "=" <> escapeForAttributeSelector text exactness <> "]")

getByAttributeSelectorRegex :: Text -> Text -> Selector
getByAttributeSelectorRegex attr source =
  selectorFromText ("internal:attr=[" <> attr <> "=" <> escapeRegexForSelector source <> "]")

getByTestIdSelector :: Text -> Selector
getByTestIdSelector testId =
  selectorFromText ("internal:testid=[data-testid=" <> escapeForAttributeSelector testId Exact <> "]")

getByTestIdSelectorRegex :: Text -> Selector
getByTestIdSelectorRegex source =
  selectorFromText ("internal:testid=[data-testid=" <> escapeRegexForSelector source <> "]")

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
        , nameAttr opts
        , boolAttr "pressed" (getByRolePressed opts)
        ]

boolAttr :: Text -> Maybe Bool -> Maybe Text
boolAttr name value =
  fmap (\flag -> "[" <> name <> "=" <> boolText flag <> "]") value

nameAttr :: GetByRoleOptions -> Maybe Text
nameAttr opts =
  case getByRoleNameRegex opts of
    Just source ->
      Just ("[name=" <> escapeRegexForSelector source <> "]")
    Nothing ->
      fmap (\name -> "[name=" <> escapeForAttributeSelector name (getByRoleExact opts) <> "]") (getByRoleName opts)

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

escapeRegexForSelector :: Text -> Text
escapeRegexForSelector source =
  replace ">>" "\\>\\>" source

exactnessSuffix :: Exactness -> Text
exactnessSuffix exactness =
  case exactness of
    Exact ->
      "s"
    Inexact ->
      "i"

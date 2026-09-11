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
import Data.Text
  ( Text
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
  ( GetByRoleOptions (getByRoleExact, getByRoleName)
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
  selectorFromText ("internal:role=" <> ariaRoleToText role <> nameAttr)
  where
    nameAttr :: Text
    nameAttr =
      case getByRoleName opts of
        Nothing ->
          ""
        Just name ->
          "[name=" <> escapeForAttributeSelector name (getByRoleExact opts) <> "]"

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

module Playwright.Codegen.Names
  ( haskellKeywords
  , isKeyword
  , pascalCase
  , camelCase
  , escapeIdent
  , literalConstructor
  , fieldName
  ) where

import Data.Char
  ( isAlphaNum
  , isDigit
  )
import Data.Set
  ( Set
  )
import Data.Set qualified as Set
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Text.Manipulate
  ( toCamel
  , toPascal
  )

haskellKeywords :: Set Text
haskellKeywords =
  Set.fromList
    [ "case"
    , "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "else"
    , "foreign"
    , "if"
    , "import"
    , "in"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "let"
    , "module"
    , "newtype"
    , "of"
    , "then"
    , "type"
    , "where"
    , "mdo"
    , "rec"
    , "pattern"
    , "role"
    , "family"
    , "forall"
    , "export"
    , "hiding"
    , "qualified"
    , "as"
    ]

isKeyword :: Text -> Bool
isKeyword = flip Set.member haskellKeywords

normalizeIdent :: Text -> Text
normalizeIdent =
  Text.map replace
  where
    replace :: Char -> Char
    replace c =
      if isAlphaNum c
        then c
        else '-'

pascalCase :: Text -> Text
pascalCase txt =
  let
    converted :: Text
    converted = toPascal (normalizeIdent txt)
  in
    if Text.null converted
      then "X"
      else converted

camelCase :: Text -> Text
camelCase txt =
  let
    converted :: Text
    converted = toCamel (normalizeIdent txt)
  in
    if Text.null converted
      then "x"
      else converted

escapeIdent :: Text -> Text
escapeIdent name =
  let
    startFixed :: Text
    startFixed =
      case Text.uncons name of
        Just (c, _) | isDigit c ->
          Text.cons 'x' name
        _ ->
          name
    keywordFixed :: Text
    keywordFixed =
      if isKeyword startFixed
        then startFixed <> "_"
        else startFixed
  in
    keywordFixed

literalConstructor :: Text -> Text -> Text
literalConstructor typeName literal =
  typeName <> pascalCase (sanitizeLiteral literal)

sanitizeLiteral :: Text -> Text
sanitizeLiteral literal =
  case literal of
    "-Infinity" ->
      "NegInfinity"
    "-0" ->
      "NegZero"
    other ->
      other

fieldName :: Text -> Text -> Text
fieldName typeName jsonName =
  escapeIdent (camelCase typeName <> pascalCase jsonName)

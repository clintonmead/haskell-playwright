module Playwright.UrlMatch
  ( UrlMatch (..)
  , matchesUrlMatch
  ) where

import Data.String
  ( IsString (fromString)
  )
import Data.Text
  ( Text
  , pack
  , unpack
  )
import Playwright.Url
  ( Url
  , urlToText
  )
import Text.Regex.Base
  ( matchTest
  )
import Text.Regex.TDFA
  ( CompOption (caseSensitive, multiline)
  , Regex
  , defaultCompOpt
  , defaultExecOpt
  , makeRegexOptsM
  )

-- | What a URL is matched against, as the TypeScript API takes
-- @string | RegExp | function@. A glob is the usual form, and a string literal
-- means a glob.
data UrlMatch
  = -- | A glob such as @**\/*.js@.
    UrlMatchGlob Text
  | -- | A regular expression as its source and flags, such as @\"\\\\d+\"@ and @\"i\"@.
    UrlMatchRegex Text Text
  | -- | A predicate on the URL. It runs on the client only.
    UrlMatchPredicate (Url -> Bool)

instance IsString UrlMatch where
  fromString = UrlMatchGlob . pack

-- | Two globs or regexes are equal when their text is. Predicates have no
-- equality, so two predicates never compare equal, as functions do not in
-- the TypeScript client.
instance Eq UrlMatch where
  UrlMatchGlob left == UrlMatchGlob right =
    left == right
  UrlMatchRegex leftSource leftFlags == UrlMatchRegex rightSource rightFlags =
    leftSource == rightSource && leftFlags == rightFlags
  _ == _ =
    False

instance Show UrlMatch where
  showsPrec precedence match =
    case match of
      UrlMatchGlob glob ->
        showParen (precedence > 10) (showString "UrlMatchGlob " . showsPrec 11 glob)
      UrlMatchRegex source flags ->
        showParen (precedence > 10) (showString "UrlMatchRegex " . showsPrec 11 source . showChar ' ' . showsPrec 11 flags)
      UrlMatchPredicate _ ->
        showString "UrlMatchPredicate <predicate>"

matchesUrlMatch :: UrlMatch -> Url -> Bool
matchesUrlMatch match url =
  case match of
    UrlMatchGlob glob ->
      globMatch (unpack glob) (unpack (urlToText url))
    UrlMatchRegex source flags ->
      regexMatches source flags (unpack (urlToText url))
    UrlMatchPredicate predicate ->
      predicate url

-- | Like JavaScript's @RegExp.prototype.test@: the pattern may match anywhere
-- in the text. An invalid pattern matches nothing.
regexMatches :: Text -> Text -> String -> Bool
regexMatches source flags text =
  case compileRegex source flags of
    Just regex ->
      matchTest regex text
    Nothing ->
      False

compileRegex :: Text -> Text -> Maybe Regex
compileRegex source flags =
  makeRegexOptsM compOptions defaultExecOpt (posixSource (unpack source))
  where
    flagSet = unpack flags
    compOptions =
      defaultCompOpt
        { caseSensitive = 'i' `notElem` flagSet
        , multiline = 'm' `elem` flagSet
        }

-- | Rewrites the JavaScript escapes that regex-tdfa lacks into bracket
-- expressions: @\\d \\D \\w \\W \\s \\S@ and an escaped slash. Other escapes are
-- left as they are.
posixSource :: String -> String
posixSource = go False
  where
    go _ [] =
      []
    go inBracket ('\\' : c : rest) =
      case escapeFor inBracket c of
        Just replacement ->
          replacement <> go inBracket rest
        Nothing ->
          '\\' : c : go inBracket rest
    go False ('[' : rest) =
      '[' : go True rest
    go True ('[' : rest) =
      '[' : go True rest
    go True (']' : rest) =
      ']' : go False rest
    go inBracket (c : rest) =
      c : go inBracket rest

escapeFor :: Bool -> Char -> Maybe String
escapeFor inBracket c =
  case c of
    'd' -> Just (if inBracket then digits else "[" <> digits <> "]")
    'w' -> Just (if inBracket then wordChars else "[" <> wordChars <> "]")
    's' -> Just (if inBracket then spaces else "[" <> spaces <> "]")
    'D' | not inBracket -> Just ("[^" <> digits <> "]")
    'W' | not inBracket -> Just ("[^" <> wordChars <> "]")
    'S' | not inBracket -> Just ("[^" <> spaces <> "]")
    '/' -> Just "/"
    _ -> Nothing

digits :: String
digits = "0-9"

wordChars :: String
wordChars = "a-zA-Z0-9_"

spaces :: String
spaces = " \t\n\r\f\v"

globMatch :: String -> String -> Bool
globMatch pattern text =
  case (pattern, text) of
    ([], []) ->
      True
    ('*' : '*' : rest, remaining) ->
      globMatch rest remaining
        || case remaining of
          [] ->
            False
          (_ : more) ->
            globMatch ('*' : '*' : rest) more
    ('*' : rest, remaining) ->
      globMatch rest remaining
        || case remaining of
          [] ->
            False
          ('/' : _) ->
            False
          (_ : more) ->
            globMatch ('*' : rest) more
    ('?' : rest, _ : more) ->
      globMatch rest more
    (p : ps, c : cs) ->
      p == c && globMatch ps cs
    _ ->
      False

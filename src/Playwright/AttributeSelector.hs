-- | Parser for attribute selectors such as @BookItem[name = "foo" i]@.
--
-- Ported from upstream @packages/isomorphic/selectorParser.ts@
-- (@parseAttributeSelector@). Error messages match upstream so callers can
-- match on the "while parsing selector" wording.
module Playwright.AttributeSelector
  ( AttributeSelector (..)
  , AttributeSelectorPart (..)
  , parseAttributeSelector
  ) where

import Control.Monad (ap, guard, liftM, unless, void, when)
import Data.Char (digitToInt, isDigit, isHexDigit, isSpace)
import Data.List (nub)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Playwright.AttributeSelectorOperator
  ( AttributeSelectorOperator
      ( OpTruthy
      , OpEqual
      , OpContains
      , OpDashMatch
      , OpPrefix
      , OpSuffix
      , OpWord
      )
  , renderOperator
  )
import Playwright.AttributeValue
  ( AttributeValue
      ( AttributeString
      , AttributeNumber
      , AttributeBool
      , AttributeRegex
      )
  , renderAttributeValue
  )

data AttributeSelector = AttributeSelector
  { attributeSelectorName :: Text
  , attributeSelectorAttributes :: [AttributeSelectorPart]
  }
  deriving stock (Eq, Show)

data AttributeSelectorPart = AttributeSelectorPart
  { attributePartName :: Text
  , attributePartJsonPath :: [Text]
  , attributePartOperator :: AttributeSelectorOperator
  , attributePartValue :: Maybe AttributeValue
  , attributePartCaseSensitive :: Bool
  }
  deriving stock (Eq, Show)

-- | Parses an attribute selector. The flag allows unquoted values that are
-- not booleans to stay strings instead of being read as numbers.
parseAttributeSelector :: Bool -> Text -> Either Text AttributeSelector
parseAttributeSelector allowUnquoted selector =
  fst <$> runParser topLevel env 0
  where
    env = Env selector (T.length selector) allowUnquoted
    topLevel = do
      name <- readIdentifier
      skipSpaces
      attrs <- attributes
      done <- atEnd
      unless done (syntaxError Nothing)
      when (T.null name && null attrs) (selectorError "selector cannot be empty")
      pure (AttributeSelector name attrs)
    attributes = do
      next <- peekChar
      if next == Just '['
        then do
          attr <- readAttribute
          skipSpaces
          rest <- attributes
          pure (attr : rest)
        else pure []

-- Parser internals

data Env = Env
  { envSelector :: Text
  , envLength :: Int
  , envAllowUnquoted :: Bool
  }

newtype Parser a = Parser {runParser :: Env -> Int -> Either Text (a, Int)}

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser (\_ i -> Right (a, i))
  (<*>) = ap

instance Monad Parser where
  Parser p >>= k = Parser $ \env i -> case p env i of
    Left err -> Left err
    Right (a, j) -> runParser (k a) env j

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \env i -> Right (charAt env i, i)

charAt :: Env -> Int -> Maybe Char
charAt env i
  | i < envLength env = Just (T.index (envSelector env) i)
  | otherwise = Nothing

atEnd :: Parser Bool
atEnd = Parser $ \env i -> Right (i >= envLength env, i)

-- | Consumes one character and returns it, or 'Nothing' at the end.
eat1 :: Parser (Maybe Char)
eat1 = Parser $ \env i -> Right (charAt env i, if i < envLength env then i + 1 else i)

skip1 :: Parser ()
skip1 = void eat1

getEnv :: Parser Env
getEnv = Parser $ \env i -> Right (env, i)

failWith :: Text -> Parser a
failWith err = Parser $ \_ _ -> Left err

-- | Reports an error the way upstream's @syntaxError@ does: an end-of-input
-- message at the end, otherwise the unexpected symbol and its position.
syntaxError :: Maybe Text -> Parser a
syntaxError stage = Parser $ \env i ->
  let sel = envSelector env
      err
        | i >= envLength env =
            "Unexpected end of selector while parsing selector `" <> sel <> "`"
        | otherwise =
            "Error while parsing selector `" <> sel <> "` - unexpected symbol \""
              <> T.singleton (T.index sel i)
              <> "\" at position " <> T.pack (show i)
              <> maybe "" (" during " <>) stage
  in Left err

selectorError :: Text -> Parser a
selectorError detail = do
  env <- getEnv
  failWith ("Error while parsing selector `" <> envSelector env <> "` - " <> detail)

skipSpaces :: Parser ()
skipSpaces = do
  done <- atEnd
  unless done $ do
    next <- peekChar
    when (maybe False isSpace next) (skip1 >> skipSpaces)

-- https://www.w3.org/TR/css-syntax-3/#ident-token-diagram
isCSSNameChar :: Char -> Bool
isCSSNameChar c =
  c >= '\x80'
    || isDigit c
    || (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || c == '_'
    || c == '-'

takeWhileP :: (Char -> Bool) -> Parser Text
takeWhileP p = T.pack <$> go
  where
    go = do
      next <- peekChar
      case next of
        Just c | p c -> do
          skip1
          rest <- go
          pure (c : rest)
        _ -> pure []

readIdentifier :: Parser Text
readIdentifier = do
  skipSpaces
  takeWhileP isCSSNameChar

-- | Reads a quoted string and returns its contents with escapes resolved
-- (a backslash keeps the character after it, as upstream does).
readQuotedString :: Char -> Parser Text
readQuotedString quote = do
  opening <- eat1
  unless (opening == Just quote) (syntaxError (Just "parsing quoted string"))
  body <- go
  closing <- peekChar
  unless (closing == Just quote) (syntaxError (Just "parsing quoted string"))
  skip1
  pure (T.pack body)
  where
    go = do
      next <- peekChar
      case next of
        Just c | c /= quote -> do
          when (c == '\\') skip1
          escaped <- eat1
          rest <- go
          pure (maybe id (:) escaped rest)
        _ -> pure []

-- | Reads @/source/flags@ and returns the source and canonically ordered flags.
readRegularExpression :: Parser (Text, Text)
readRegularExpression = do
  opening <- eat1
  unless (opening == Just '/') (syntaxError (Just "parsing regular expression"))
  source <- sourceLoop False
  closing <- eat1
  unless (closing == Just '/') (syntaxError (Just "parsing regular expression"))
  flags <- takeWhileP (`elem` ("dgimsuvy" :: String))
  validateFlags flags
  pure (T.pack source, T.pack [f | f <- "dgimsuvy", f `elem` T.unpack flags])
  where
    sourceLoop inClass = do
      done <- atEnd
      if done
        then pure []
        else do
          next <- peekChar
          case next of
            Just '\\' -> do
              skip1
              atLast <- atEnd
              when atLast (syntaxError (Just "parsing regular expression"))
              escaped <- eat1
              rest <- sourceLoop inClass
              pure ('\\' : maybe [] pure escaped ++ rest)
            Just ']' | inClass -> consume ']' False
            Just '[' | not inClass -> consume '[' True
            Just '/' | not inClass -> pure []
            Just c -> consume c inClass
            Nothing -> pure []
    consume c inClass = do
      skip1
      rest <- sourceLoop inClass
      pure (c : rest)

validateFlags :: Text -> Parser ()
validateFlags flags = do
  let fs = T.unpack flags
      duplicated = length (nub fs) /= length fs
      unicodeBoth = 'u' `elem` fs && 'v' `elem` fs
  when (duplicated || unicodeBoth) $ do
    env <- getEnv
    failWith
      ( "Error while parsing selector `" <> envSelector env
          <> "`: Invalid flags supplied to RegExp constructor '" <> flags <> "'"
      )

readAttributeToken :: Parser Text
readAttributeToken = do
  skipSpaces
  next <- peekChar
  token <- case next of
    Just q | q == '\'' || q == '"' -> readQuotedString q
    _ -> readIdentifier
  when (T.null token) (syntaxError (Just "parsing property path"))
  pure token

-- | Reads the remaining @.token@ segments of a property path.
readPathTail :: Parser [Text]
readPathTail = do
  skipSpaces
  next <- peekChar
  if next == Just '.'
    then do
      skip1
      token <- readAttributeToken
      rest <- readPathTail
      pure (token : rest)
    else pure []

readOperator :: Parser AttributeSelectorOperator
readOperator = do
  skipSpaces
  first <- eat1
  op <- case first of
    Nothing -> pure ""
    Just '=' -> pure "="
    Just c -> do
      done <- atEnd
      if done
        then pure [c]
        else do
          second <- eat1
          pure (c : maybe "" pure second)
  case op of
    "=" -> pure OpEqual
    "*=" -> pure OpContains
    "^=" -> pure OpPrefix
    "$=" -> pure OpSuffix
    "|=" -> pure OpDashMatch
    "~=" -> pure OpWord
    _ -> syntaxError (Just "parsing operator")

-- | Reads the value after an operator, returning it with its case sensitivity.
readValue :: AttributeSelectorOperator -> Parser (AttributeValue, Bool)
readValue op = do
  skipSpaces
  next <- peekChar
  case next of
    Just '/' -> do
      when (op /= OpEqual)
        (selectorError ("cannot use " <> renderOperator op <> " in attribute with regular expression"))
      (source, flags) <- readRegularExpression
      pure (AttributeRegex source flags, True)
    Just q | q == '\'' || q == '"' -> do
      str <- readQuotedString q
      skipSpaces
      flag <- peekChar
      case flag of
        Just f | f == 'i' || f == 'I' -> skip1 >> pure (AttributeString str, False)
               | f == 's' || f == 'S' -> skip1 >> pure (AttributeString str, True)
        _ -> pure (AttributeString str, True)
    _ -> do
      raw <- takeWhileP (\c -> isCSSNameChar c || c == '+' || c == '.')
      env <- getEnv
      case raw of
        "true" -> pure (AttributeBool True, True)
        "false" -> pure (AttributeBool False, True)
        _
          | envAllowUnquoted env -> pure (AttributeString raw, True)
          | otherwise -> case jsNumber raw of
              Just n -> pure (AttributeNumber n, True)
              Nothing -> syntaxError (Just "parsing attribute value")

readAttribute :: Parser AttributeSelectorPart
readAttribute = do
  skip1
  first <- readAttributeToken
  path <- (first :) <$> readPathTail
  skipSpaces
  next <- peekChar
  if next == Just ']'
    then do
      skip1
      pure (part path OpTruthy Nothing False)
    else do
      op <- readOperator
      (value, caseSensitive) <- readValue op
      skipSpaces
      close <- peekChar
      unless (close == Just ']') (syntaxError (Just "parsing attribute value"))
      skip1
      let nonString = case value of
            AttributeString _ -> False
            _ -> True
      when (op /= OpEqual && nonString) $
        selectorError
          ( "cannot use " <> renderOperator op
              <> " in attribute with non-string matching value - " <> renderAttributeValue value
          )
      pure (part path op (Just value) caseSensitive)
  where
    part path op value caseSensitive =
      AttributeSelectorPart (T.intercalate "." path) path op value caseSensitive

-- | Converts the characters of an unquoted value the way JavaScript's
-- @Number(text)@ does for the characters this parser can collect. An empty
-- value is zero. Returns 'Nothing' where JavaScript would give NaN.
jsNumber :: Text -> Maybe Scientific
jsNumber raw
  | T.null raw = Just 0
  | otherwise = case T.unpack raw of
      '0' : marker : digits
        | Just radix <- lookup marker radixMarkers -> radixValue radix digits
      str -> decimalValue str

radixMarkers :: [(Char, Integer)]
radixMarkers = [('x', 16), ('X', 16), ('o', 8), ('O', 8), ('b', 2), ('B', 2)]

radixValue :: Integer -> String -> Maybe Scientific
radixValue radix digits
  | null digits = Nothing
  | not (all valid digits) = Nothing
  | otherwise = Just (fromInteger (foldl' step 0 digits))
  where
    valid c = isHexDigit c && toInteger (digitToInt c) < radix
    step acc c = acc * radix + toInteger (digitToInt c)

decimalValue :: String -> Maybe Scientific
decimalValue str = do
  let negative = take 1 str == "-"
      unsigned = case str of
        s : rest | s == '-' || s == '+' -> rest
        _ -> str
      (intDigits, afterInt) = span isDigit unsigned
  (fracDigits, afterFrac) <- case afterInt of
    '.' : rest -> Just (span isDigit rest)
    _ -> Just ("", afterInt)
  guard (not (null intDigits && null fracDigits))
  exponentValue' <- case afterFrac of
    [] -> Just 0
    e : rest | e == 'e' || e == 'E' -> exponentOf rest
    _ -> Nothing
  let coefficient = digitsToInteger (intDigits ++ fracDigits)
      value = scientific coefficient (exponentValue' - length fracDigits)
  pure (if negative then negate value else value)

exponentOf :: String -> Maybe Int
exponentOf str = do
  let (negative, unsigned) = case str of
        '-' : rest -> (True, rest)
        '+' : rest -> (False, rest)
        _ -> (False, str)
      (ds, leftover) = span isDigit unsigned
  guard (not (null ds) && null leftover)
  let n :: Int
      n = fromInteger (digitsToInteger ds)
  pure (if negative then negate n else n)

digitsToInteger :: String -> Integer
digitsToInteger = foldl' (\acc c -> acc * 10 + toInteger (digitToInt c)) 0

-- | Port of upstream @packages/isomorphic@ @selectorParser.ts@,
-- @locatorGenerators.ts@ and @locatorParser.ts@.
--
-- Converts internal selector strings (for example
-- @internal:role=button[name="Save"i]@) into locator code for JavaScript,
-- Python, Java and C#, and parses such locator code back into a selector.
-- The JSONL generator of upstream is not ported.
module Playwright.LocatorGenerator
  ( Language (..)
  , Quote (..)
  , asLocator
  , asLocators
  , asLocatorsWith
  , asLocatorDescription
  , locatorOrSelectorAsSelector
  ) where

import Control.Monad
  ( foldM
  , unless
  , when
  )
import Data.Aeson
  ( Value (Number, String)
  , eitherDecodeStrict
  )
import Data.Char
  ( isAsciiLower
  , isAsciiUpper
  , isDigit
  , isSpace
  , ord
  , toLower
  , toUpper
  )
import Data.Foldable
  ( toList
  )
import Data.List
  ( intercalate
  , isPrefixOf
  , isSuffixOf
  , stripPrefix
  )
import Data.Maybe
  ( fromMaybe
  , listToMaybe
  )
import Data.Text qualified as T
import Data.Text.Encoding
  ( encodeUtf8
  )
import Numeric
  ( showHex
  )
import Text.Read
  ( readMaybe
  )
import Text.Regex.Base.RegexLike
  ( makeRegex
  , matchAll
  )
import Text.Regex.TDFA
  ( Regex
  )

-- ---------------------------------------------------------------------------
-- Public API

data Language = JavaScript | Python | Java | CSharp
  deriving stock (Eq, Show)

data Quote = SingleQuote | DoubleQuote | BacktickQuote
  deriving stock (Eq, Show)

-- | Locator for a selector (the first candidate), as upstream @asLocator@.
asLocator :: Language -> T.Text -> T.Text
asLocator lang selector =
  T.pack (firstOr input (asLocatorsString lang False 1 SingleQuote input))
  where
    input = T.unpack selector

-- | All locator candidates for a selector (at most 20), as upstream @asLocators@.
asLocators :: Language -> T.Text -> [T.Text]
asLocators lang selector = asLocatorsWith lang False 20 Nothing selector

-- | As 'asLocators' with the frame-locator flag, the output limit and the
-- preferred quote.
asLocatorsWith :: Language -> Bool -> Int -> Maybe Quote -> T.Text -> [T.Text]
asLocatorsWith lang isFrame maxOut preferred selector =
  map T.pack (asLocatorsString lang isFrame maxOut (fromMaybe SingleQuote preferred) (T.unpack selector))

-- | Human readable description of a selector, as upstream @asLocatorDescription@.
asLocatorDescription :: Language -> T.Text -> T.Text
asLocatorDescription lang selector =
  T.pack (asLocatorDescriptionString lang (T.unpack selector))

-- | Converts a locator (or a selector) back into a selector. Returns the input
-- when it already is a selector, the parsed selector when the locator round
-- trips, and the empty text otherwise.
locatorOrSelectorAsSelector :: Language -> T.Text -> T.Text -> T.Text
locatorOrSelectorAsSelector lang locator testIdAttr =
  T.pack (locatorOrSelectorString lang (T.unpack locator) (T.unpack testIdAttr))

-- ---------------------------------------------------------------------------
-- Small helpers

firstOr :: a -> [a] -> a
firstOr def xs = fromMaybe def (listToMaybe xs)

digitsInt :: String -> Int
digitsInt s = fromMaybe 0 (readMaybe s)

-- | All matches of a pattern. Each match is the list of (offset, length) of
-- the whole match followed by its groups (offset -1 when a group did not
-- participate).
regexMatches :: String -> String -> [[(Int, Int)]]
regexMatches pat input = map toList (matchAll (makeRegex pat :: Regex) input)

sliceAt :: String -> (Int, Int) -> String
sliceAt input (offset, len) =
  if offset < 0 then "" else take len (drop offset input)

-- | Global replacement with a regular expression, like JavaScript
-- @String.replace(/pat/g, fn)@. The function receives the captured groups.
gsub :: String -> ([String] -> String) -> String -> String
gsub pat render input = go 0 (regexMatches pat input)
  where
    go pos [] = drop pos input
    go pos (groups : more) = case groups of
      [] -> go pos more
      (start, len) : rest ->
        take (start - pos) (drop pos input)
          <> render (map (sliceAt input) rest)
          <> go (start + len) more

-- | Global replacement of a literal string.
replaceLit :: String -> String -> String -> String
replaceLit old new = go
  where
    go [] = []
    go s@(c : cs)
      | not (null old) && old `isPrefixOf` s = new <> go (drop (length old) s)
      | otherwise = c : go cs

-- | Replacement of the first occurrence of a literal string.
replaceFirstLit :: String -> String -> String -> String
replaceFirstLit old new = go
  where
    go [] = []
    go s@(c : cs)
      | old `isPrefixOf` s = new <> drop (length old) s
      | otherwise = c : go cs

applyAll :: [String -> String] -> String -> String
applyAll fs s = foldl (\acc f -> f acc) s fs

showNumber :: Double -> String
showNumber d
  | fromIntegral whole == d = show whole
  | otherwise = show d
  where
    whole :: Integer
    whole = round d

boolText :: Bool -> String
boolText b = if b then "true" else "false"

-- | JSON.stringify for a string.
jsonQuote :: String -> String
jsonQuote text = "\"" <> concatMap escape text <> "\""
  where
    escape c = case c of
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      _
        | ord c < 0x20 -> "\\u" <> pad4 (showHex (ord c) "")
        | otherwise -> [c]
    pad4 h = replicate (4 - length h) '0' <> h

decodeValues :: String -> Either String [Value]
decodeValues text = eitherDecodeStrict (encodeUtf8 (T.pack text))

-- | Decodes a JSON string literal.
jsonString :: String -> Either String String
jsonString text = case decodeValues ("[" <> text <> "]") of
  Right [String value] -> Right (T.unpack value)
  _ -> Left "Expected a JSON string"

-- | @escapeWithQuotes@ from upstream stringUtils.
escapeWithQuotes :: String -> Char -> String
escapeWithQuotes text quote =
  [quote] <> concatMap escapeChar (replaceLit "\\\"" "\"" (inner (jsonQuote text))) <> [quote]
  where
    inner :: String -> String
    inner s = drop 1 (take (length s - 1) s)
    escapeChar :: Char -> String
    escapeChar c = if c == quote then ['\\', c] else [c]

-- | @normalizeEscapedRegexQuotes@ from upstream stringUtils.
normalizeEscapedRegexQuotes :: String -> String
normalizeEscapedRegexQuotes = go
  where
    go :: String -> String
    go [] = []
    go s@('\\' : _) =
      let (backslashes, rest) = span (== '\\') s
          count = length backslashes
       in case rest of
            (q : rest')
              | q `elem` quoteChars && odd count ->
                  replicate (count - 1) '\\' <> [q] <> go rest'
            _ -> backslashes <> go rest
    go (c : cs) = c : go cs

quoteChars :: String
quoteChars = "\"'`"

-- | @escapeRegexForSelector@ from upstream stringUtils, applied to the
-- @String(re)@ form of a regular expression.
escapeRegexSelector :: String -> String -> String
escapeRegexSelector source flags
  | any (`elem` ("uv" :: String)) flags = text
  | otherwise = replaceLit ">>" "\\>\\>" (insertQuoteEscapes text)
  where
    text = "/" <> source <> "/" <> flags
    insertQuoteEscapes :: String -> String
    insertQuoteEscapes = go (0 :: Int)
      where
        go :: Int -> String -> String
        go _ [] = []
        go run (c : cs)
          | c == '\\' = c : go (run + 1) cs
          | c `elem` quoteChars && even run = '\\' : c : go 0 cs
          | otherwise = c : go 0 cs

-- | @escapeForTextSelector@ for a string.
escapeText :: String -> Bool -> String
escapeText text exact = jsonQuote text <> (if exact then "s" else "i")

-- | @escapeForAttributeSelector@ for a string.
escapeAttributeValue :: String -> Bool -> String
escapeAttributeValue value exact =
  "\"" <> concatMap escape value <> "\"" <> (if exact then "s" else "i")
  where
    escape c = case c of
      '\\' -> "\\\\"
      '"' -> "\\\""
      _ -> [c]

encodeTestIdAttributeName :: String -> String
encodeTestIdAttributeName name = if ',' `elem` name then jsonQuote name else name

-- | @toSnakeCase@ from upstream stringUtils.
toSnakeCase :: String -> String
toSnakeCase = map toLower . pass2 . pass1
  where
    pass1 :: String -> String
    pass1 (a : b : rest)
      | isLowerOrDigit a && isAsciiUpper b = a : '_' : b : pass1 rest
    pass1 (a : rest) = a : pass1 rest
    pass1 [] = []
    pass2 :: String -> String
    pass2 (x : y : z : rest)
      | isAsciiUpper x && isAsciiUpper y && isAsciiLower z = x : '_' : y : z : pass2 rest
    pass2 (x : rest) = x : pass2 rest
    pass2 [] = []
    isLowerOrDigit c = isAsciiLower c || isDigit c

-- | @toTitleCase@ from upstream stringUtils.
toTitleCase :: String -> String
toTitleCase s = case s of
  (c : cs) -> toUpper c : cs
  [] -> []

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- ---------------------------------------------------------------------------
-- Selector parsing (selectorParser.ts)

data SelPart = SelPart
  { partName :: String
  , partSource :: String
  , partNested :: Maybe Nested
  }

-- | A nested selector body (for internal:has, left-of, ...) and its distance.
data Nested = Nested ParsedSelector (Maybe Double)

data ParsedSelector = ParsedSelector
  { parsedParts :: [SelPart]
  , parsedCapture :: Maybe Int
  }

nestedNames :: [String]
nestedNames =
  [ "internal:has"
  , "internal:has-not"
  , "internal:and"
  , "internal:or"
  , "internal:chain"
  , "left-of"
  , "right-of"
  , "above"
  , "below"
  , "near"
  ]

distanceNames :: [String]
distanceNames = ["left-of", "right-of", "above", "below", "near"]

parseSelector :: String -> Either String ParsedSelector
parseSelector selector = do
  (raw, capture) <- parseSelectorChunks selector
  reversedParts <- foldM convertPart [] raw
  let parts = reverse reversedParts
  case parts of
    (p : _) | partName p `elem` nestedNames ->
      Left ("\"" <> partName p <> "\" selector cannot be first")
    _ -> pure ()
  pure (ParsedSelector parts capture)

parseSelectorChunks :: String -> Either String ([(String, String)], Maybe Int)
parseSelectorChunks selector = go (0 :: Int) Nothing [] (splitTop selector)
  where
    go :: Int -> Maybe Int -> [(String, String)] -> [String] -> Either String ([(String, String)], Maybe Int)
    go _ capture acc [] = Right (reverse acc, capture)
    go idx capture acc (chunk : more) = do
      let trimmed = trim chunk
          (isCapture, body0) = case trimmed of
            ('*' : rest) -> (True, rest)
            _ -> (False, trimmed)
          (name, body) = classify (trim body0)
      capture' <-
        if not isCapture
          then Right capture
          else case capture of
            Just _ -> Left "Only one of the selectors can capture using * modifier"
            Nothing -> Right (Just idx)
      go (idx + 1) capture' ((name, body) : acc) more

-- | Splits a selector at ">>" tokens that are outside quotes.
splitTop :: String -> [String]
splitTop = go [] Nothing
  where
    go :: String -> Maybe Char -> String -> [String]
    go cur _ [] = [reverse cur]
    go cur quote ('\\' : c : rest) = go (c : '\\' : cur) quote rest
    go cur (Just q) (c : rest)
      | c == q = go (c : cur) Nothing rest
      | otherwise = go (c : cur) (Just q) rest
    go cur Nothing ('>' : '>' : rest) = reverse cur : go [] Nothing rest
    go cur Nothing (c : rest)
      | c `elem` quoteChars && not (ignoreQuote cur) = go (c : cur) (Just c) rest
      | otherwise = go (c : cur) Nothing rest

-- | A quote that follows @text=@ with some text after the equals sign is
-- literal text, not the start of a quoted section.
ignoreQuote :: String -> Bool
ignoreQuote reversedPrefix =
  case stripWord "text" (dropWhile isSpace (reverse reversedPrefix)) of
    Just afterText -> case dropWhile isSpace afterText of
      ('=' : rest) -> not (null (dropWhile isSpace rest))
      _ -> False
    Nothing -> False
  where
    stripWord :: String -> String -> Maybe String
    stripWord w s = if w `isPrefixOf` s then Just (drop (length w) s) else Nothing

-- | Splits a chunk into its engine name and body.
classify :: String -> (String, String)
classify chunk = case break (== '=') chunk of
  (lhs, '=' : _)
    | isEngineName (trim lhs) -> (trim lhs, drop 1 (dropWhile (/= '=') chunk))
  _
    | isQuotedText chunk -> ("text", chunk)
    | "//" `isPrefixOf` chunk || ".." `isPrefixOf` chunk -> ("xpath", chunk)
    | otherwise -> ("css", chunk)
  where
    isEngineName :: String -> Bool
    isEngineName n = not (null n) && all isEngineChar n
    isEngineChar :: Char -> Bool
    isEngineChar c = isAttrNameChar c || c `elem` ("+:*" :: String)
    isQuotedText :: String -> Bool
    isQuotedText (q : rest) = q `elem` ("\"'" :: String) && not (null rest) && endsWith q rest
    isQuotedText [] = False
    endsWith :: Char -> String -> Bool
    endsWith q rest = case reverse rest of
      (c : _) -> c == q
      [] -> False

convertPart :: [SelPart] -> (String, String) -> Either String [SelPart]
convertPart acc (name, body)
  | name == "css" = pure (SelPart "css" body Nothing : acc)
  | name == "css:light" = pure (SelPart "css" (":light(" <> body <> ")") Nothing : acc)
  | name `elem` nestedNames = do
      (innerText, distance) <- nestedBody name body
      inner <- parseSelector innerText
      stripped <- stripSharedFrames (reverse acc) inner
      pure (SelPart name body (Just (Nested stripped distance)) : acc)
  | otherwise = pure (SelPart name body Nothing : acc)

nestedBody :: String -> String -> Either String (String, Maybe Double)
nestedBody name body = case decodeValues ("[" <> body <> "]") of
  Right [String inner] -> Right (T.unpack inner, Nothing)
  Right [String inner, Number d]
    | name `elem` distanceNames -> Right (T.unpack inner, Just (realToFrac d))
  _ -> Left ("Malformed selector: " <> name <> "=" <> body)

-- | Removes the frame prefix of a nested selector when it repeats the frame
-- prefix of the outer selector.
stripSharedFrames :: [SelPart] -> ParsedSelector -> Either String ParsedSelector
stripSharedFrames outer inner = case lastIndex (map isEnterFrame parts) of
  Just idx
    | stringifyParts False Nothing (take (idx + 1) parts) == stringifyParts False Nothing outer ->
        case parsedCapture inner of
          Just cap
            | cap <= idx ->
                Left "Can not capture the selector before diving into the frame. Only use * after the last frame has been selected"
            | otherwise -> Right (ParsedSelector (drop (idx + 1) parts) (Just (cap - idx - 1)))
          Nothing -> Right (ParsedSelector (drop (idx + 1) parts) Nothing)
  _ -> Right inner
  where
    parts = parsedParts inner
    isEnterFrame p = partName p == "internal:control" && partSource p == "enter-frame"
    lastIndex xs = foldl (\acc (i, b) -> if b then Just i else acc) Nothing (zip [0 :: Int ..] xs)

-- | Renders parts back into selector text.
stringifyParts :: Bool -> Maybe Int -> [SelPart] -> String
stringifyParts force capture parts =
  intercalate " >> " (zipWith render [0 :: Int ..] parts)
  where
    render i p =
      let isCaptured = Just i == capture
          withEngine =
            force
              || isCaptured
              || not (partName p == "css" || (partName p == "xpath" && isRelativeXPath (partSource p)))
          prefix = if withEngine then partName p <> "=" else ""
       in (if isCaptured then "*" else "") <> prefix <> partSource p
    isRelativeXPath s = "//" `isPrefixOf` s || ".." `isPrefixOf` s

-- ---------------------------------------------------------------------------
-- Attribute selector parsing

data AttrValue
  = AVString String
  | AVNumber Double
  | AVBool Bool
  | AVRegex String String

data AttrPart = AttrPart
  { attrPartName :: String
  , attrPartValue :: Maybe AttrValue
  , attrPartCaseSensitive :: Bool
  }

newtype AttrSelector = AttrSelector
  { attrSelectorAttributes :: [AttrPart]
  }

isAttrNameChar :: Char -> Bool
isAttrNameChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("_-" :: String) || c >= '\x80'

attrError :: Either String a
attrError = Left "Error while parsing attribute selector"

-- | Parses a body such as @button[name="Save"i][level=3]@ into its role name
-- and attributes.
parseAttributeSelector :: String -> Either String (String, AttrSelector)
parseAttributeSelector sel = do
  let (name, afterName) = span isAttrNameChar (dropWhile isSpace sel)
  (attrs, rest) <- attrLoop afterName
  unless (all isSpace rest) attrError
  when (null name && null attrs) (Left ("Error while parsing selector `" <> sel <> "` - selector cannot be empty"))
  pure (name, AttrSelector attrs)
  where
    attrLoop s = case dropWhile isSpace s of
      ('[' : more) -> do
        (attr, more') <- readAttribute more
        (attrs', final) <- attrLoop more'
        pure (attr : attrs', final)
      other -> pure ([], other)

readAttribute :: String -> Either String (AttrPart, String)
readAttribute input = do
  (path, afterPath) <- readPath input
  let name = intercalate "." path
  case dropWhile isSpace afterPath of
    (']' : afterClose) -> pure (AttrPart name Nothing True, afterClose)
    other -> do
      (op, afterOp) <- readOperator (dropWhile isSpace other)
      (value, caseSensitive, afterValue) <- readValue op (dropWhile isSpace afterOp)
      case dropWhile isSpace afterValue of
        (']' : afterClose) -> do
          when (op /= "=" && not (isStringValue value)) attrError
          pure (AttrPart name (Just value) caseSensitive, afterClose)
        _ -> attrError
  where
    isStringValue (AVString _) = True
    isStringValue _ = False

readPath :: String -> Either String ([String], String)
readPath input = do
  (token, afterToken) <- readAttrToken input
  case dropWhile isSpace afterToken of
    ('.' : more) -> do
      (tokens, final) <- readPath more
      pure (token : tokens, final)
    other -> pure ([token], other)

readAttrToken :: String -> Either String (String, String)
readAttrToken input = case dropWhile isSpace input of
  (q : more)
    | q == '\'' || q == '"' -> maybe attrError Right (readQuoted q more)
  other -> case span isAttrNameChar other of
    ([], _) -> attrError
    (token, rest) -> Right (token, rest)

-- | Reads a quoted string whose opening quote was already consumed.
readQuoted :: Char -> String -> Maybe (String, String)
readQuoted q = go []
  where
    go _ [] = Nothing
    go acc ('\\' : c : rest) = go (c : acc) rest
    go acc (c : rest)
      | c == q = Just (reverse acc, rest)
      | otherwise = go (c : acc) rest

readOperator :: String -> Either String (String, String)
readOperator input = case input of
  ('=' : rest) -> Right ("=", rest)
  (c : '=' : rest)
    | c `elem` ("*^$|~" :: String) -> Right ([c, '='], rest)
  _ -> attrError

readValue :: String -> String -> Either String (AttrValue, Bool, String)
readValue op input = case input of
  ('/' : _)
    | op /= "=" -> attrError
    | otherwise -> case readRegex input of
        Just (source, flags, rest) -> Right (AVRegex source flags, True, rest)
        Nothing -> attrError
  (q : more)
    | q == '\'' || q == '"' -> case readQuoted q more of
        Nothing -> attrError
        Just (str, rest) -> case rest of
          ('i' : rest') -> Right (AVString str, False, rest')
          ('s' : rest') -> Right (AVString str, True, rest')
          _ -> Right (AVString str, True, rest)
  _ -> case span isUnquotedChar input of
    ("true", rest) -> Right (AVBool True, True, rest)
    ("false", rest) -> Right (AVBool False, True, rest)
    ([], _) -> attrError
    (value, rest) -> case readMaybe value :: Maybe Double of
      Just n | all isDigit value -> Right (AVNumber n, True, rest)
      _ -> Right (AVString value, True, rest)
  where
    isUnquotedChar c = isAttrNameChar c || c `elem` ("+." :: String)

readRegex :: String -> Maybe (String, String, String)
readRegex ('/' : input) = go [] False input
  where
    go _ _ [] = Nothing
    go acc inClass ('\\' : c : rest) = go (c : '\\' : acc) inClass rest
    go acc True (']' : rest) = go (']' : acc) False rest
    go acc False ('[' : rest) = go ('[' : acc) True rest
    go acc False ('/' : rest) =
      let (flags, rest') = span (`elem` ("dgimsuvy" :: String)) rest
       in Just (reverse acc, flags, rest')
    go acc inClass (c : rest) = go (c : acc) inClass rest
readRegex _ = Nothing

-- ---------------------------------------------------------------------------
-- Locator generation (locatorGenerators.ts)

-- | A pattern argument: a text, or a regular expression (source and flags).
data Pat = PText String | PRe String String

patText :: Pat -> String
patText (PText s) = s
patText (PRe s _) = s

isTextPat :: Pat -> Bool
isTextPat (PText _) = True
isTextPat (PRe _ _) = False

data Base = BasePage | BaseLocator | BaseFrameLocator

data Kind
  = KDefault
  | KFrameLocator
  | KFrame
  | KAnyFrame
  | KNth
  | KFirst
  | KLast
  | KVisible
  | KFilterVisible
  | KRole
  | KHasText
  | KHasNotText
  | KHas
  | KHasNot
  | KAnd
  | KOr
  | KChain
  | KTestId
  | KText
  | KAlt
  | KPlaceholder
  | KLabel
  | KTitle

data Opts = Opts
  { optExact :: Bool
  , optName :: Maybe Pat
  , optDescription :: Maybe Pat
  , optHasText :: Maybe Pat
  , optHasNotText :: Maybe Pat
  , optAttrs :: [(String, AttrValue)]
  }

noOpts :: Opts
noOpts = Opts False Nothing Nothing Nothing Nothing []

type Gen = Base -> Kind -> Pat -> Opts -> String

quoteChar :: Quote -> Char
quoteChar q = case q of
  SingleQuote -> '\''
  DoubleQuote -> '"'
  BacktickQuote -> '`'

quoteFromChar :: Char -> Maybe Quote
quoteFromChar c = case c of
  '\'' -> Just SingleQuote
  '"' -> Just DoubleQuote
  '`' -> Just BacktickQuote
  _ -> Nothing

jsRegex :: String -> String -> String
jsRegex source flags = normalizeEscapedRegexQuotes ("/" <> source <> "/" <> flags)

pyRegex :: String -> String -> String
pyRegex source flags = "re.compile(r\"" <> inner <> "\"" <> suffix <> ")"
  where
    fixedSlashes = replaceFirstLit "\\/" "/" (normalizeEscapedRegexQuotes source)
    inner = concatMap (\c -> if c == '"' then "\\\"" else [c]) fixedSlashes
    suffix :: String
    suffix = if 'i' `elem` flags then ", re.IGNORECASE" else ""

javaRegex :: String -> String -> String
javaRegex source flags = "Pattern.compile(" <> escapeWithQuotes (normalizeEscapedRegexQuotes source) '"' <> suffix <> ")"
  where
    suffix :: String
    suffix = if 'i' `elem` flags then ", Pattern.CASE_INSENSITIVE" else ""

csRegex :: String -> String -> String
csRegex source flags = "new Regex(" <> escapeWithQuotes (normalizeEscapedRegexQuotes source) '"' <> suffix <> ")"
  where
    suffix :: String
    suffix = if 'i' `elem` flags then ", RegexOptions.IgnoreCase" else ""

-- | Generates the locator text for one selector part kind.
generate :: Language -> Quote -> Gen
generate lang quote base kind body opts = case lang of
  JavaScript -> genJs quote base kind body opts
  Python -> genPy kind body opts
  Java -> genJava base kind body opts
  CSharp -> genCSharp kind body opts

-- | Chains locator candidates (every language joins them with a dot).
chainLocators :: [String] -> String
chainLocators = intercalate "."

genJs :: Quote -> Base -> Kind -> Pat -> Opts -> String
genJs quote _base kind body opts = case kind of
  KDefault
    | Just hasText <- optHasText opts -> "locator(" <> quoted bodyS <> ", { hasText: " <> jsPat hasText <> " })"
    | Just hasNotText <- optHasNotText opts -> "locator(" <> quoted bodyS <> ", { hasNotText: " <> jsPat hasNotText <> " })"
    | otherwise -> "locator(" <> quoted bodyS <> ")"
  KFrameLocator -> "frameLocator(" <> quoted bodyS <> ")"
  KFrame -> "contentFrame()"
  KAnyFrame -> "frameLocator()"
  KNth -> "nth(" <> bodyS <> ")"
  KFirst -> "first()"
  KLast -> "last()"
  KVisible -> "visible()"
  KFilterVisible -> "filter({ visible: " <> bodyS <> " })"
  KRole -> "getByRole(" <> quoted bodyS <> roleArgs <> ")"
  KHasText -> "filter({ hasText: " <> jsPat body <> " })"
  KHasNotText -> "filter({ hasNotText: " <> jsPat body <> " })"
  KHas -> "filter({ has: " <> bodyS <> " })"
  KHasNot -> "filter({ hasNot: " <> bodyS <> " })"
  KAnd -> "and(" <> bodyS <> ")"
  KOr -> "or(" <> bodyS <> ")"
  KChain -> "locator(" <> bodyS <> ")"
  KTestId -> "getByTestId(" <> jsPat body <> ")"
  KText -> jsCall "getByText"
  KAlt -> jsCall "getByAltText"
  KPlaceholder -> jsCall "getByPlaceholder"
  KLabel -> jsCall "getByLabel"
  KTitle -> jsCall "getByTitle"
  where
    bodyS = patText body
    quoted s = escapeWithQuotes s (quoteChar quote)
    jsPat (PText s) = quoted s
    jsPat (PRe s f) = jsRegex s f
    jsCall method = case body of
      PRe s f -> method <> "(" <> jsRegex s f <> ")"
      PText s
        | optExact opts -> method <> "(" <> quoted s <> ", { exact: true })"
        | otherwise -> method <> "(" <> quoted s <> ")"
    roleArgs =
      let nameArgs = maybe [] (\p -> ["name: " <> jsPat p]) (optName opts)
          descArgs = maybe [] (\p -> ["description: " <> jsPat p]) (optDescription opts)
          exactArgs :: [String]
          exactArgs = ["exact: true" | optExact opts && hasTextName]
          attrArgs = [key <> ": " <> jsValue v | (key, v) <- optAttrs opts]
          items = nameArgs <> descArgs <> exactArgs <> attrArgs
       in if null items then "" else ", { " <> intercalate ", " items <> " }"
    hasTextName = or [isTextPat p | Just p <- [optName opts, optDescription opts]]
    jsValue v = case v of
      AVString s -> quoted s
      AVBool b -> boolText b
      AVNumber n -> showNumber n
      AVRegex s f -> jsRegex s f

genPy :: Kind -> Pat -> Opts -> String
genPy kind body opts = case kind of
  KDefault
    | Just hasText <- optHasText opts -> "locator(" <> pyQ bodyS <> ", has_text=" <> pyPat hasText <> ")"
    | Just hasNotText <- optHasNotText opts -> "locator(" <> pyQ bodyS <> ", has_not_text=" <> pyPat hasNotText <> ")"
    | otherwise -> "locator(" <> pyQ bodyS <> ")"
  KFrameLocator -> "frame_locator(" <> pyQ bodyS <> ")"
  KFrame -> "content_frame"
  KAnyFrame -> "frame_locator()"
  KNth -> "nth(" <> bodyS <> ")"
  KFirst -> "first"
  KLast -> "last"
  KVisible -> "visible"
  KFilterVisible -> "filter(visible=" <> pyBool (bodyS == "true") <> ")"
  KRole -> "get_by_role(" <> pyQ bodyS <> roleArgs <> ")"
  KHasText -> "filter(has_text=" <> pyPat body <> ")"
  KHasNotText -> "filter(has_not_text=" <> pyPat body <> ")"
  KHas -> "filter(has=" <> bodyS <> ")"
  KHasNot -> "filter(has_not=" <> bodyS <> ")"
  KAnd -> "and_(" <> bodyS <> ")"
  KOr -> "or_(" <> bodyS <> ")"
  KChain -> "locator(" <> bodyS <> ")"
  KTestId -> "get_by_test_id(" <> pyPat body <> ")"
  KText -> pyCall "get_by_text"
  KAlt -> pyCall "get_by_alt_text"
  KPlaceholder -> pyCall "get_by_placeholder"
  KLabel -> pyCall "get_by_label"
  KTitle -> pyCall "get_by_title"
  where
    bodyS = patText body
    pyQ s = escapeWithQuotes s '"'
    pyBool :: Bool -> String
    pyBool b = if b then "True" else "False"
    pyPat (PText s) = pyQ s
    pyPat (PRe s f) = pyRegex s f
    pyCall method = case body of
      PRe s f -> method <> "(" <> pyRegex s f <> ")"
      PText s
        | optExact opts -> method <> "(" <> pyQ s <> ", exact=True)"
        | otherwise -> method <> "(" <> pyQ s <> ")"
    roleArgs =
      let nameArgs = maybe [] (\p -> ["name=" <> pyPat p]) (optName opts)
          descArgs = maybe [] (\p -> ["description=" <> pyPat p]) (optDescription opts)
          exactArgs :: [String]
          exactArgs = ["exact=True" | optExact opts && hasTextName]
          attrArgs = [toSnakeCase key <> "=" <> pyValue v | (key, v) <- optAttrs opts]
          items = nameArgs <> descArgs <> exactArgs <> attrArgs
       in if null items then "" else ", " <> intercalate ", " items
    hasTextName = or [isTextPat p | Just p <- [optName opts, optDescription opts]]
    pyValue v = case v of
      AVString s -> pyQ s
      AVBool b -> pyBool b
      AVNumber n -> showNumber n
      AVRegex s f -> pyRegex s f

genJava :: Base -> Kind -> Pat -> Opts -> String
genJava base kind body opts = case kind of
  KDefault
    | Just hasText <- optHasText opts -> "locator(" <> javaQ bodyS <> ", new " <> clazz <> ".LocatorOptions().setHasText(" <> javaPat hasText <> "))"
    | Just hasNotText <- optHasNotText opts -> "locator(" <> javaQ bodyS <> ", new " <> clazz <> ".LocatorOptions().setHasNotText(" <> javaPat hasNotText <> "))"
    | otherwise -> "locator(" <> javaQ bodyS <> ")"
  KFrameLocator -> "frameLocator(" <> javaQ bodyS <> ")"
  KFrame -> "contentFrame()"
  KAnyFrame -> "frameLocator()"
  KNth -> "nth(" <> bodyS <> ")"
  KFirst -> "first()"
  KLast -> "last()"
  KVisible -> "visible()"
  KFilterVisible -> "filter(new " <> clazz <> ".FilterOptions().setVisible(" <> javaBool (bodyS == "true") <> "))"
  KRole -> "getByRole(AriaRole." <> map toUpper (toSnakeCase bodyS) <> roleArgs <> ")"
  KHasText -> "filter(new " <> clazz <> ".FilterOptions().setHasText(" <> javaPat body <> "))"
  KHasNotText -> "filter(new " <> clazz <> ".FilterOptions().setHasNotText(" <> javaPat body <> "))"
  KHas -> "filter(new " <> clazz <> ".FilterOptions().setHas(" <> bodyS <> "))"
  KHasNot -> "filter(new " <> clazz <> ".FilterOptions().setHasNot(" <> bodyS <> "))"
  KAnd -> "and(" <> bodyS <> ")"
  KOr -> "or(" <> bodyS <> ")"
  KChain -> "locator(" <> bodyS <> ")"
  KTestId -> "getByTestId(" <> javaPat body <> ")"
  KText -> javaCall "getByText"
  KAlt -> javaCall "getByAltText"
  KPlaceholder -> javaCall "getByPlaceholder"
  KLabel -> javaCall "getByLabel"
  KTitle -> javaCall "getByTitle"
  where
    bodyS = patText body
    clazz :: String
    clazz = case base of
      BasePage -> "Page"
      BaseFrameLocator -> "FrameLocator"
      BaseLocator -> "Locator"
    javaQ s = escapeWithQuotes s '"'
    javaPat (PText s) = javaQ s
    javaPat (PRe s f) = javaRegex s f
    javaBool :: Bool -> String
    javaBool b = if b then "true" else "false"
    javaCall method = case body of
      PRe s f -> method <> "(" <> javaRegex s f <> ")"
      PText s
        | optExact opts -> method <> "(" <> javaQ s <> ", new " <> clazz <> "." <> toTitleCase method <> "Options().setExact(true))"
        | otherwise -> method <> "(" <> javaQ s <> ")"
    roleArgs =
      let nameArgs = maybe [] (\p -> [".setName(" <> javaPat p <> ")"]) (optName opts)
          descArgs = maybe [] (\p -> [".setDescription(" <> javaPat p <> ")"]) (optDescription opts)
          exactArgs :: [String]
          exactArgs = [".setExact(true)" | optExact opts && hasTextName]
          attrArgs = [".set" <> toTitleCase key <> "(" <> javaValue v <> ")" | (key, v) <- optAttrs opts]
          items = nameArgs <> descArgs <> exactArgs <> attrArgs
       in if null items then "" else ", new " <> clazz <> ".GetByRoleOptions()" <> concat items
    hasTextName = or [isTextPat p | Just p <- [optName opts, optDescription opts]]
    javaValue v = case v of
      AVString s -> javaQ s
      AVBool b -> javaBool b
      AVNumber n -> showNumber n
      AVRegex s f -> javaRegex s f

genCSharp :: Kind -> Pat -> Opts -> String
genCSharp kind body opts = case kind of
  KDefault
    | Just hasText <- optHasText opts -> "Locator(" <> cq bodyS <> ", new() { " <> csHasText hasText <> " })"
    | Just hasNotText <- optHasNotText opts -> "Locator(" <> cq bodyS <> ", new() { " <> csHasNotText hasNotText <> " })"
    | otherwise -> "Locator(" <> cq bodyS <> ")"
  KFrameLocator -> "FrameLocator(" <> cq bodyS <> ")"
  KFrame -> "ContentFrame"
  KAnyFrame -> "FrameLocator()"
  KNth -> "Nth(" <> bodyS <> ")"
  KFirst -> "First"
  KLast -> "Last"
  KVisible -> "Visible"
  KFilterVisible -> "Filter(new() { Visible = " <> csBool (bodyS == "true") <> " })"
  KRole -> "GetByRole(AriaRole." <> toTitleCase bodyS <> roleArgs <> ")"
  KHasText -> "Filter(new() { " <> csHasText body <> " })"
  KHasNotText -> "Filter(new() { " <> csHasNotText body <> " })"
  KHas -> "Filter(new() { Has = " <> bodyS <> " })"
  KHasNot -> "Filter(new() { HasNot = " <> bodyS <> " })"
  KAnd -> "And(" <> bodyS <> ")"
  KOr -> "Or(" <> bodyS <> ")"
  KChain -> "Locator(" <> bodyS <> ")"
  KTestId -> "GetByTestId(" <> csPat body <> ")"
  KText -> csCall "GetByText"
  KAlt -> csCall "GetByAltText"
  KPlaceholder -> csCall "GetByPlaceholder"
  KLabel -> csCall "GetByLabel"
  KTitle -> csCall "GetByTitle"
  where
    bodyS = patText body
    cq s = escapeWithQuotes s '"'
    csBool :: Bool -> String
    csBool b = if b then "true" else "false"
    csPat (PText s) = cq s
    csPat (PRe s f) = csRegex s f
    csHasText (PText s) = "HasText = " <> cq s
    csHasText (PRe s f) = "HasTextRegex = " <> csRegex s f
    csHasNotText (PText s) = "HasNotText = " <> cq s
    csHasNotText (PRe s f) = "HasNotTextRegex = " <> csRegex s f
    csCall method = case body of
      PRe s f -> method <> "(" <> csRegex s f <> ")"
      PText s
        | optExact opts -> method <> "(" <> cq s <> ", new() { Exact = true })"
        | otherwise -> method <> "(" <> cq s <> ")"
    roleArgs =
      let nameArgs = maybe [] (csNamed "Name" "NameRegex") (optName opts)
          descArgs = maybe [] (csNamed "Description" "DescriptionRegex") (optDescription opts)
          exactArgs :: [String]
          exactArgs = ["Exact = true" | optExact opts && hasTextName]
          attrArgs = [toTitleCase key <> " = " <> csValue v | (key, v) <- optAttrs opts]
          items = nameArgs <> descArgs <> exactArgs <> attrArgs
       in if null items then "" else ", new() { " <> intercalate ", " items <> " }"
    csNamed :: String -> String -> Pat -> [String]
    csNamed plain _ (PText s) = [plain <> " = " <> cq s]
    csNamed _ regexName (PRe s f) = [regexName <> " = " <> csRegex s f]
    hasTextName = or [isTextPat p | Just p <- [optName opts, optDescription opts]]
    csValue v = case v of
      AVString s -> cq s
      AVBool b -> csBool b
      AVNumber n -> showNumber n
      AVRegex s f -> csRegex s f

-- ---------------------------------------------------------------------------
-- Candidate combination (innerAsLocators and combineTokens)

asLocatorsString :: Language -> Bool -> Int -> Quote -> String -> [String]
asLocatorsString lang isFrame maxOut quote selector =
  either (const [selector]) id (parseSelector selector >>= \parsed -> innerAsLocators lang quote parsed isFrame maxOut)

asLocatorDescriptionString :: Language -> String -> String
asLocatorDescriptionString lang selector = either (const selector) id describe
  where
    describe = do
      parsed <- parseSelector selector
      custom <- customDescription parsed
      case custom of
        Just text -> pure text
        Nothing -> do
          rows <- innerAsLocators lang SingleQuote parsed False 1
          case rows of
            (row : _) -> pure row
            [] -> Left "no locator"

customDescription :: ParsedSelector -> Either String (Maybe String)
customDescription parsed = case reverse (parsedParts parsed) of
  (part : _) | partName part == "internal:describe" -> case jsonString (partSource part) of
    Right text -> Right (Just text)
    Left err -> Left err
  _ -> Right Nothing

innerAsLocators :: Language -> Quote -> ParsedSelector -> Bool -> Int -> Either String [String]
innerAsLocators lang quote parsed isFrame maxOut = do
  rows <- collectTokens lang quote maxOut (parsedParts parsed) Nothing [] (if isFrame then BaseFrameLocator else BasePage)
  pure (combineTokens (reverse rows) maxOut)

-- | Cartesian product of the candidates for each part, limited to @maxOut@.
combineTokens :: [[String]] -> Int -> [String]
combineTokens rows maxOut = take (max 1 maxOut) (map chainLocators (sequence rows))

-- | Walks the selector parts and collects the candidate list for each part
-- (most recent first).
collectTokens :: Language -> Quote -> Int -> [SelPart] -> Maybe SelPart -> [[String]] -> Base -> Either String [[String]]
collectTokens lang quote maxOut = go
  where
    gen :: Gen
    gen = generate lang quote
    go :: [SelPart] -> Maybe SelPart -> [[String]] -> Base -> Either String [[String]]
    go [] _ acc _ = pure acc
    go (part : rest) prev acc base = case partName part of
      "internal:describe" -> go rest (Just part) acc BaseLocator
      "nth" -> emit (nthRows gen base (partSource part))
      "visible" -> emit (visibleRows gen base (partSource part))
      "internal:text" -> do
        (exact, pat) <- detectExact (partSource part)
        emit [gen base KText pat noOpts {optExact = exact}]
      "internal:has-text" -> do
        (exact, pat) <- detectExact (partSource part)
        if exact then defaultPart else emit [gen base KHasText pat noOpts]
      "internal:has-not-text" -> do
        (exact, pat) <- detectExact (partSource part)
        if exact then defaultPart else emit [gen base KHasNotText pat noOpts]
      "internal:has" -> nested KHas
      "internal:has-not" -> nested KHasNot
      "internal:and" -> nested KAnd
      "internal:or" -> nested KOr
      "internal:chain" -> nested KChain
      "internal:label" -> do
        (exact, pat) <- detectExact (partSource part)
        emit [gen base KLabel pat noOpts {optExact = exact}]
      "internal:role" -> do
        (roleName, sel) <- parseAttributeSelector (partSource part)
        (exactSeen, opts) <- foldM (roleAttr part) (Nothing, noOpts) (attrSelectorAttributes sel)
        emit [gen base KRole (PText roleName) opts {optExact = fromMaybe False exactSeen}]
      "internal:testid" -> do
        attr <- firstAttr (partSource part)
        emit [gen base KTestId (attrPat (attrPartValue attr)) noOpts]
      "internal:attr" -> do
        attr <- firstAttr (partSource part)
        let exact = attrPartCaseSensitive attr
            pat = attrPat (attrPartValue attr)
        case attrPartName attr of
          "placeholder" -> emit [gen base KPlaceholder pat noOpts {optExact = exact}]
          "alt" -> emit [gen base KAlt pat noOpts {optExact = exact}]
          "title" -> emit [gen base KTitle pat noOpts {optExact = exact}]
          _ -> defaultPart
      "internal:control"
        | partSource part == "any-frame" ->
            go rest (Just part) ([gen base KAnyFrame (PText "") noOpts] : acc) BaseFrameLocator
        | partSource part == "enter-frame" -> enterFrame
      _ -> defaultPart
      where
        emit rows = go rest (Just part) (rows : acc) BaseLocator
        nested kind = case partNested part of
          Nothing -> Left "Missing nested selector"
          Just (Nested inner _) -> do
            inners <- innerAsLocators lang quote inner False maxOut
            emit [gen base kind (PText i) noOpts | i <- inners]
        firstAttr text = do
          (_, sel) <- parseAttributeSelector text
          case attrSelectorAttributes sel of
            (attr : _) -> Right attr
            [] -> Left "Missing attribute"
        enterFrame = case (prev, acc) of
          (Just prevPart, lastRows : accRest) -> do
            let frameRows = [chainLocators [row, gen base KFrame (PText "") noOpts] | row <- lastRows]
                engineRows =
                  if partName prevPart `elem` ["xpath", "css"]
                    then
                      [ gen base KFrameLocator (PText (stringifyParts False Nothing [prevPart])) noOpts
                      , gen base KFrameLocator (PText (stringifyParts True Nothing [prevPart])) noOpts
                      ]
                    else []
            go rest (Just part) ((frameRows <> engineRows) : accRest) BaseFrameLocator
          _ -> Left "enter-frame without a preceding selector part"
        defaultPart = do
          let selectorPart = stringifyParts False Nothing [part]
              locatorPart = gen base KDefault (PText selectorPart) noOpts
              plainRows =
                if partName part `elem` ["xpath", "css"]
                  then [locatorPart, gen base KDefault (PText (stringifyParts True Nothing [part])) noOpts]
                  else [locatorPart]
              plain = go rest (Just part) (plainRows : acc) BaseLocator
          case rest of
            (next : rest')
              | partName next `elem` ["internal:has-text", "internal:has-not-text"] -> do
                  (exact, pat) <- detectExact (partSource next)
                  if exact
                    then plain
                    else do
                      let hasText = partName next == "internal:has-text"
                          nextKind = if hasText then KHasText else KHasNotText
                          nextLocatorPart = gen BaseLocator nextKind pat noOpts
                          opts =
                            if hasText
                              then noOpts {optHasText = Just pat}
                              else noOpts {optHasNotText = Just pat}
                          combined = gen base KDefault (PText selectorPart) opts
                      go rest' (Just next) ([chainLocators [locatorPart, nextLocatorPart], combined] : acc) BaseLocator
            _ -> plain

-- | Candidates for @nth=@ parts.
nthRows :: Gen -> Base -> String -> [String]
nthRows gen base index
  | index == "0" = [gen base KFirst (PText "") noOpts, gen base KNth (PText index) noOpts]
  | index == "-1" = [gen base KLast (PText "") noOpts, gen base KNth (PText index) noOpts]
  | otherwise = [gen base KNth (PText index) noOpts]

-- | Candidates for @visible=@ parts.
visibleRows :: Gen -> Base -> String -> [String]
visibleRows gen base body =
  [gen base KVisible (PText "") noOpts | body == "true"]
    <> [ gen base KFilterVisible (PText body) noOpts
       , gen base KDefault (PText ("visible=" <> body)) noOpts
       ]

-- | Folds one attribute of an @internal:role@ selector into the options.
roleAttr :: SelPart -> (Maybe Bool, Opts) -> AttrPart -> Either String (Maybe Bool, Opts)
roleAttr part (exactSoFar, opts) attr = case attrPartName attr of
  "name" -> named (\o p -> o {optName = Just p})
  "description" -> named (\o p -> o {optDescription = Just p})
  key -> pure (exactSoFar, opts {optAttrs = optAttrs opts <> [(keyOf key, attrValueOf key)]})
  where
    cs = attrPartCaseSensitive attr
    named :: (Opts -> Pat -> Opts) -> Either String (Maybe Bool, Opts)
    named setter = case exactSoFar of
      Just e | e /= cs ->
        Left ("Conflicting exactness in internal:role selector: " <> stringifyParts False Nothing [part])
      _ -> pure (Just cs, setter opts (attrPat (attrPartValue attr)))
    keyOf :: String -> String
    keyOf k = if k == "include-hidden" then "includeHidden" else k
    attrValueOf :: String -> AttrValue
    attrValueOf k = case attrPartValue attr of
      Nothing -> AVBool True
      Just (AVString s) | k == "level" -> maybe (AVString s) AVNumber (readMaybe s)
      Just v -> v

attrPat :: Maybe AttrValue -> Pat
attrPat (Just (AVRegex s f)) = PRe s f
attrPat (Just (AVString s)) = PText s
attrPat (Just (AVBool b)) = PText (boolText b)
attrPat (Just (AVNumber n)) = PText (showNumber n)
attrPat Nothing = PText ""

-- | Upstream @detectExact@: a regular expression, or a text that is exact
-- when quoted with a trailing @"@ or @"s@.
detectExact :: String -> Either String (Bool, Pat)
detectExact text = case regexParts text of
  Just (source, flags) -> Right (False, PRe source flags)
  Nothing
    | "\"" `isSuffixOf` text -> do
        decoded <- jsonString text
        Right (True, PText decoded)
    | "\"s" `isSuffixOf` text -> do
        decoded <- jsonString (dropLast text)
        Right (True, PText decoded)
    | "\"i" `isSuffixOf` text -> do
        decoded <- jsonString (dropLast text)
        Right (False, PText decoded)
    | otherwise -> Right (False, PText text)
  where
    dropLast :: String -> String
    dropLast s = take (length s - 1) s

-- | Matches @^\/(.*)\/([igm]*)$@.
regexParts :: String -> Maybe (String, String)
regexParts text = case text of
  ('/' : _) | closeIdx >= 1 && closeSlash -> Just (take (closeIdx - 1) (drop 1 text), reverse flagsRev)
  _ -> Nothing
  where
    flagsRev = takeWhile (`elem` ("igm" :: String)) (reverse text)
    closeIdx = length text - length flagsRev - 1
    closeSlash = case drop closeIdx text of
      ('/' : _) -> True
      _ -> False

-- ---------------------------------------------------------------------------
-- Locator parsing (locatorParser.ts)

-- | Converts locator code into a selector template and its preferred quote.
parseLocatorText :: String -> String -> (String, Maybe Quote)
parseLocatorText locator0 testIdAttr = (transform template1 params testIdAttr, preferred)
  where
    locator1 =
      gsub
        "AriaRole[ \t\n]*\\.[ \t\n]*([A-Za-z0-9_]+)"
        (\g -> map toLower (firstOr "" g))
        locator0
    locator =
      gsub
        "(get_by_role|getByRole)[ \t\n]*\\([ \t\n]*[\"'`]([^'\"`]+)['\"`]"
        (\g -> case g of
          [a, b] -> a <> "(" <> map toLower b
          _ -> "")
        locator1
    (template0, params) = tokenize locator
    template1 = normalizeTemplate template0
    preferred = listToMaybe [q | (q, _) <- params, q `elem` quoteChars] >>= quoteFromChar

-- | Splits locator text into a template (with @$N@ parameter references) and
-- the quoted parameters.
tokenize :: String -> (String, [(Char, String)])
tokenize = go (1 :: Int) Nothing
  where
    go :: Int -> Maybe Char -> String -> (String, [(Char, String)])
    go _ _ [] = ([], [])
    go n prev (c : rest)
      | c `notElem` ("\"'`/" :: String) =
          let (t, ps) = go n (Just c) rest
           in (c : t, ps)
      | otherwise =
          let isRegex = prev == Just 'r' || c == '/'
              (text, after) = readBody isRegex c rest
              (t, ps) = go (n + 1) (Just c) after
              ref = (if c == '/' then "r" else "") <> "$" <> show n
           in (ref <> t, (c, text) : ps)

-- | Reads a quoted body up to the closing quote. Returns the decoded text and
-- the input after the closing quote.
readBody :: Bool -> Char -> String -> (String, String)
readBody isRegex q = go []
  where
    go acc [] = (reverse acc, [])
    go acc ('\\' : more)
      | isRegex = case more of
          (n : more') -> go (n : (if n /= q then '\\' : acc else acc)) more'
          [] -> (reverse acc, [])
      | otherwise = case more of
          (n : more') -> go (unescape n : acc) more'
          [] -> (reverse acc, [])
    go acc (c : more)
      | c == q = (reverse acc, more)
      | otherwise = go (c : acc) more
    unescape n = case n of
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'
      other -> other

-- | Language normalization of the template, mapping Python, Java and C#
-- spellings onto the JavaScript-like form.
normalizeTemplate :: String -> String
normalizeTemplate t0 = applyAll steps (map toLower t0)
  where
    steps =
      [ replaceLit "get_by_alt_text" "getbyalttext"
      , replaceLit "get_by_test_id" "getbytestid"
      , gsub "get_by_([A-Za-z0-9_]+)" (\g -> "getby" <> firstOr "" g)
      , replaceLit "has_not_text" "hasnottext"
      , replaceLit "has_text" "hastext"
      , replaceLit "has_not" "hasnot"
      , replaceLit "frame_locator" "framelocator"
      , replaceLit "content_frame" "contentframe"
      , filter (\c -> c `notElem` ("{}" :: String) && not (isSpace c))
      , replaceLit "new()" ""
      , gsub "new[A-Za-z0-9_]+\\.[A-Za-z0-9_]+options\\(\\)" (const "")
      , replaceLit ".set" ",set"
      , replaceLit ".or_(" "or("
      , replaceLit ".and_(" "and("
      , replaceLit ":" "="
      , replaceLit ",re.ignorecase" "i"
      , replaceLit ",pattern.case_insensitive" "i"
      , replaceLit ",regexoptions.ignorecase" "i"
      , gsub "re.compile\\(([^)]+)\\)" (firstOr "")
      , gsub "pattern.compile\\(([^)]+)\\)" (\g -> "r" <> firstOr "" g)
      , gsub "newregex\\(([^)]+)\\)" (\g -> "r" <> firstOr "" g)
      , replaceLit "string=" "="
      , replaceLit "regex=" "="
      , replaceLit ",," ","
      , replaceLit ",)" ")"
      ]

-- | Number of @$N@ references in a template.
countParams :: String -> Int
countParams s = length [() | (c, d) <- zip s (drop 1 s), c == '$', isDigit d]

-- | Rewrites @$N@ references (no @r@ prefix) through a function.
shiftParams :: String -> Int -> String
shiftParams s sub = go s
  where
    go str = case str of
      [] -> []
      ('$' : more) | (ds@(_ : _), rest) <- span isDigit more ->
        "$" <> show (digitsInt ds - sub) <> go rest
      (c : cs) -> c : go cs

-- | Finds the first @filter(,?has=|hasnot=|sethas(|sethasnot()@. Returns the
-- match start, the index just after the keyword and the keyword.
findFilterHas :: String -> Maybe (Int, Int, String)
findFilterHas = go 0
  where
    keywords :: [String]
    keywords = ["has=", "hasnot=", "sethas(", "sethasnot("]
    go :: Int -> String -> Maybe (Int, Int, String)
    go i str = case stripPrefix "filter(" str of
      Just after ->
        let (commaLen, rest) = case after of
              (',' : more) -> (1 :: Int, more)
              _ -> (0 :: Int, after)
         in case [k | k <- keywords, k `isPrefixOf` rest] of
              (k : _) -> Just (i, i + length ("filter(" :: String) + commaLen + length k, k)
              [] -> next
      Nothing -> next
      where
        next = case str of
          [] -> Nothing
          (_ : more) -> go (i + 1) more

-- | Index of the parenthesis that closes the group opened before the text.
findClose :: String -> Int
findClose = scan 0 (0 :: Int)
  where
    scan :: Int -> Int -> String -> Int
    scan idx bal (c : cs)
      | c == '(' = scan (idx + 1) (bal + 1) cs
      | c == ')' = if bal == 0 then idx else scan (idx + 1) (bal - 1) cs
      | otherwise = scan (idx + 1) bal cs
    scan idx _ [] = idx

-- | Recursively handles @filter(has=...)@ and friends, moving the inner
-- locator into a parameter.
hasLoop :: String -> [(Char, String)] -> String -> (String, [(Char, String)])
hasLoop tmpl ps testId = case findFilterHas tmpl of
  Nothing -> (tmpl, ps)
  Just (_, start, keyword) ->
    let beforeStart = take start tmpl
        restAfter = drop start tmpl
        endIdx = findClose restAfter
        inner = take endIdx restAfter
        isSethas = keyword `elem` ["sethas(", "sethasnot("]
        extraSymbol = if isSethas then 1 else 0 :: Int
        prefixStr =
          if isSethas
            then replaceSuffixLit "sethasnot(" "hasnot=" (replaceSuffixLit "sethas(" "has=" beforeStart)
            else beforeStart
        countBefore = countParams beforeStart
        hasTmpl = shiftParams inner countBefore
        inHas = countParams hasTmpl
        hasParams = take inHas (drop countBefore ps)
        hasSel = jsonQuote (transform hasTmpl hasParams testId)
        tailAfter = drop extraSymbol (drop endIdx restAfter)
        newTmpl = equalsToTwo prefixStr <> "$" <> show (countBefore + 1) <> shiftParams tailAfter (inHas - 1)
        newParams = take countBefore ps <> [('"', hasSel)] <> drop (countBefore + inHas) ps
     in hasLoop newTmpl newParams testId
  where
    replaceSuffixLit :: String -> String -> String -> String
    replaceSuffixLit old new s =
      if old `isSuffixOf` s then take (length s - length old) s <> new else s
    equalsToTwo s = if "=" `isSuffixOf` s then init s <> "2=" else s

-- | Turns the parsed template into an engine selector.
transform :: String -> [(Char, String)] -> String -> String
transform template0 params0 testId =
  intercalate " >> " (map (substPart params) parts)
  where
    (template1, params) = hasLoop template0 params0 testId
    template2 = applyAll (engineSteps testId) template1
    parts = swapEnterFrames (splitDots template2)

-- | Splits on '.', like JavaScript @split('.')@.
splitDots :: String -> [String]
splitDots s = case break (== '.') s of
  (a, []) -> [a]
  (a, _ : b) -> a : splitDots b

-- | Moves @internal:control=enter-frame@ after a following @nth=@ part.
swapEnterFrames :: [String] -> [String]
swapEnterFrames list = case list of
  (a : b : rest)
    | a == "internal:control=enter-frame" && "nth=" `isPrefixOf` b -> b : swapEnterFrames (a : rest)
    | otherwise -> a : swapEnterFrames (b : rest)
  _ -> list

-- | The engine rewrites applied to the template after the has-loop.
engineSteps :: String -> [String -> String]
engineSteps testId =
  [ gsub ",set([A-Za-z0-9_]+)\\(([^)]+)\\)" (\g -> case g of
      [a, b] -> "," <> map toLower a <> "=" <> map toLower b
      _ -> "")
  , replaceLit "framelocator()" "internal:control=any-frame"
  , gsub "framelocator\\(([^)]+)\\)" (\g -> firstOr "" g <> ".internal:control=enter-frame")
  , gsub "contentframe(\\(\\))?" (const "internal:control=enter-frame")
  , gsub "locator\\(([^)]+),hastext=([^),]+)\\)" hasTextLocator
  , gsub "locator\\(([^)]+),hasnottext=([^),]+)\\)" hasNotTextLocator
  , gsub "locator\\(([^)]+)\\)" (firstOr "")
  , gsub "getbyrole\\(([^)]+)\\)" (\g -> "internal:role=" <> firstOr "" g)
  , gsub "getbytext\\(([^)]+)\\)" (\g -> "internal:text=" <> firstOr "" g)
  , gsub "getbylabel\\(([^)]+)\\)" (\g -> "internal:label=" <> firstOr "" g)
  , gsub "getbytestid\\(([^)]+)\\)" (\g -> "internal:testid=[" <> encodeTestIdAttributeName testId <> "=" <> firstOr "" g <> "]")
  , gsub "getby(placeholder|alt|title)(text)?\\(([^)]+)\\)" (\g -> case g of
      [a, _, c] -> "internal:attr=[" <> a <> "=" <> c <> "]"
      _ -> "")
  , gsub "first(\\(\\))?" (const "nth=0")
  , gsub "last(\\(\\))?" (const "nth=-1")
  , gsub "nth\\(([^)]+)\\)" (\g -> "nth=" <> firstOr "" g)
  , gsub "filter\\(,?visible=true\\)" (const "visible=true")
  , gsub "filter\\(,?visible=false\\)" (const "visible=false")
  , dotVisible
  , gsub "filter\\(,?hastext=([^)]+)\\)" (\g -> "internal:has-text=" <> firstOr "" g)
  , gsub "filter\\(,?hasnottext=([^)]+)\\)" (\g -> "internal:has-not-text=" <> firstOr "" g)
  , gsub "filter\\(,?has2=([^)]+)\\)" (\g -> "internal:has=" <> firstOr "" g)
  , gsub "filter\\(,?hasnot2=([^)]+)\\)" (\g -> "internal:has-not=" <> firstOr "" g)
  , replaceLit ",exact=false" ""
  , gsub "(,name=\\$[0-9]+)(,description=\\$[0-9]+),exact=true" (\g -> case g of
      [a, b] -> a <> "s" <> b <> "s"
      _ -> "")
  , replaceLit ",exact=true" "s"
  , replaceLit ",includehidden=" ",include-hidden="
  , replaceLit "," "]["
  ]
  where
    hasTextLocator :: [String] -> String
    hasTextLocator g = case g of
      [a, b] -> "locator(" <> a <> ").internal:has-text=" <> b
      _ -> ""
    hasNotTextLocator :: [String] -> String
    hasNotTextLocator g = case g of
      [a, b] -> "locator(" <> a <> ").internal:has-not-text=" <> b
      _ -> ""

-- | Rewrites @.visible@ and @.visible()@ (not followed by @=@) to @.visible=true@.
dotVisible :: String -> String
dotVisible s = case s of
  [] -> []
  ('.' : rest)
    | Just r <- stripPrefix "visible()" rest, notEquals r -> ".visible=true" <> dotVisible r
    | Just r <- stripPrefix "visible" rest, notEquals r -> ".visible=true" <> dotVisible r
  (c : cs) -> c : dotVisible cs
  where
    notEquals r = take 1 r /= "="

-- | Substitutes parameters into one selector part.
substPart :: [(Char, String)] -> String -> String
substPart params t
  | not ("internal:" `isPrefixOf` t) || t == "internal:control" =
      substParams (\n _ -> paramText n) t
  | otherwise =
      let t1 = if '[' `elem` t then replaceFirstLit "]" "" t <> "]" else t
          t2 = substRegexRefs t1
       in substParams (\n suf ->
            let text = paramText n
                exact = suf == Just 's'
             in if "internal:has=" `isPrefixOf` t || "internal:has-not=" `isPrefixOf` t
                  then text
                  else
                    if "internal:testid" `isPrefixOf` t
                      then escapeAttributeValue text True
                      else
                        if attrLike
                          then escapeAttributeValue text exact
                          else escapeText text exact) t2
  where
    paramText n = fromMaybe "" (listToMaybe (drop (n - 1) (map snd params)))
    attrLike = "internal:attr" `isPrefixOf` t || "internal:testid" `isPrefixOf` t || "internal:role" `isPrefixOf` t
    -- Regex references: r$N with optional i suffix.
    substRegexRefs s = case s of
      [] -> []
      ('r' : '$' : more) | (ds@(_ : _), rest) <- span isDigit more ->
        let n = digitsInt ds
            suffix :: String
            rest' :: String
            (suffix, rest') = case rest of
              ('i' : r) -> ("i", r)
              _ -> ("", rest)
            text = paramText n
            rendered
              | attrLike = escapeRegexSelector text "" <> suffix
              | otherwise = escapeRegexSelector text suffix
         in rendered <> substRegexRefs rest'
      (c : cs) -> c : substRegexRefs cs

-- | Substitutes @$N@ references (with an optional @i@ or @s@ suffix).
substParams :: (Int -> Maybe Char -> String) -> String -> String
substParams f s = case s of
  [] -> []
  ('$' : more) | (ds@(_ : _), rest) <- span isDigit more ->
    case rest of
      (x : rest') | x `elem` ("is" :: String) -> f (digitsInt ds) (Just x) <> substParams f rest'
      _ -> f (digitsInt ds) Nothing <> substParams f rest
  (c : cs) -> c : substParams f cs

-- | Converts a locator or selector to a selector. Returns the input when it
-- is already a selector, and the empty text when no conversion round trips.
locatorOrSelectorString :: Language -> String -> String -> String
locatorOrSelectorString lang locator testIdAttr =
  case parseSelector locator of
    Right _ -> locator
    Left _ ->
      let (selector, preferred) = parseLocatorText locator testIdAttr
          candidates = asLocatorsString lang False 20 (fromMaybe SingleQuote preferred) selector
          digest = digestForComparison lang locator
       in if any (\c -> digestForComparison lang c == digest) candidates
            then selector
            else ""

-- | Normalizes text for comparing a locator with a regenerated locator.
digestForComparison :: Language -> String -> String
digestForComparison lang locator =
  let stripped = filter (not . isSpace) locator
   in if lang == JavaScript
        then replaceLit ",{}" "" (normalizeQuotes stripped)
        else stripped
  where
    normalizeQuotes s = case s of
      ('\\' : q : rest) | q `elem` ("\"`" :: String) -> '\'' : normalizeQuotes rest
      (q : rest) | q `elem` ("\"`" :: String) -> '\'' : normalizeQuotes rest
      (c : rest) -> c : normalizeQuotes rest
      [] -> []

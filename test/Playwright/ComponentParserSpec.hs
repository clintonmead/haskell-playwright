module Playwright.ComponentParserSpec
  ( tests
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Playwright.AttributeSelector
  ( AttributeSelector (attributeSelectorName, attributeSelectorAttributes)
  , AttributeSelectorPart
      ( attributePartJsonPath
      , attributePartOperator
      , attributePartValue
      , attributePartCaseSensitive
      )
  , parseAttributeSelector
  )
import Playwright.AttributeSelectorOperator
  ( AttributeSelectorOperator (OpTruthy)
  , renderOperator
  )
import Playwright.AttributeValue
  ( AttributeValue
      ( AttributeString
      , AttributeNumber
      , AttributeBool
      , AttributeRegex
      )
  , renderNumber
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( Assertion
  , assertFailure
  , testCase
  , (@?=)
  )

-- Ports tests/library/component-parser.spec.ts. The upstream cases exercise
-- the attribute-selector parser (`parseAttributeSelector` in
-- packages/isomorphic/selectorParser.ts) directly, so the Haskell port calls
-- Playwright.AttributeSelector and serializes the result the same way the
-- upstream spec does.

tests :: TestTree
tests =
  testGroup
    "component-parser"
    [ testCase "should parse" testParse
    , testCase "should parse short attributes" testParseShortAttributes
    , testCase "should parse all operators" testParseAllOperators
    , testCase "should tolerate spacing" testToleratesSpacing
    , testCase "should escape" testEscape
    , testCase "should parse int values" testParseIntValues
    , testCase "should parse float values" testParseFloatValues
    , testCase "should parse bool" testParseBool
    , testCase "should parse regex" testParseRegex
    , testCase "should parse identifiers" testParseIdentifiers
    , testCase "should parse unquoted string" testParseUnquotedString
    , testCase "should throw on malformed selector" testThrowOnMalformed
    ]

-- Helpers mirroring the upstream spec's `parse`, `serialize` and `expectError`.

parse :: Text -> Either Text AttributeSelector
parse = parseAttributeSelector False

serialize :: AttributeSelector -> Text
serialize sel =
  attributeSelectorName sel <> T.concat (map attributeText (attributeSelectorAttributes sel))

attributeText :: AttributeSelectorPart -> Text
attributeText attr =
  case attributePartOperator attr of
    OpTruthy -> "[" <> path <> "]"
    op ->
      "[" <> path <> " " <> renderOperator op <> " " <> valueText
        <> (if attributePartCaseSensitive attr then "]" else " i]")
  where
    path = T.intercalate "." (map pathToken (attributePartJsonPath attr))
    valueText = case attributePartValue attr of
      Just (AttributeRegex source flags) -> "/" <> source <> "/" <> flags
      Just (AttributeString s) -> jsonString s
      Just (AttributeNumber n) -> renderNumber n
      Just (AttributeBool b) -> if b then "true" else "false"
      Nothing -> "null"

pathToken :: Text -> Text
pathToken token
  | not (T.null token) && T.all isAsciiAlnum token = token
  | otherwise = jsonString token
  where
    isAsciiAlnum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

jsonString :: Text -> Text
jsonString s = "\"" <> T.concatMap escape s <> "\""
  where
    escape c
      | c == '"' = "\\\""
      | c == '\\' = "\\\\"
      | otherwise = T.singleton c

expectSerialized :: Text -> Text -> Assertion
expectSerialized input expected = case parse input of
  Left err -> assertFailure (T.unpack err)
  Right sel -> serialize sel @?= expected

expectSerializedUnquoted :: Text -> Text -> Assertion
expectSerializedUnquoted input expected = case parseAttributeSelector True input of
  Left err -> assertFailure (T.unpack err)
  Right sel -> serialize sel @?= expected

expectError :: Text -> Assertion
expectError selector = case parse selector of
  Left err
    | T.isInfixOf ("while parsing selector `" <> selector <> "`") err -> pure ()
    | otherwise -> assertFailure ("unexpected error message: " <> T.unpack err)
  Right _ -> assertFailure ("expected a parse error for " <> T.unpack selector)

-- Cases

testParse :: Assertion
testParse = do
  expectSerialized "[foo=\"]\"]" "[foo = \"]\"]"
  expectSerialized "[foo=\"10\"s]" "[foo = \"10\"]"
  expectSerialized "[foo=\"10\" s]" "[foo = \"10\"]"
  expectSerialized "[foo=\"true\"]" "[foo = \"true\"]"
  expectSerialized "[foo=\"\"]" "[foo = \"\"]"
  expectSerialized "[foo=\"==\"]" "[foo = \"==\"]"

testParseShortAttributes :: Assertion
testParseShortAttributes = do
  expectSerialized "BookItem [  name ]" "BookItem[name]"
  expectSerialized "BookItem ['name' ] [ foo.\"bar\".baz ]" "BookItem[name][foo.bar.baz]"
  expectSerialized "BookItem ['na  me' ]" "BookItem[\"na  me\"]"

testParseAllOperators :: Assertion
testParseAllOperators = do
  expectSerialized "BookItem[name = 10]" "BookItem[name = 10]"
  expectSerialized "BookItem[name = 'foo']" "BookItem[name = \"foo\"]"
  expectSerialized "BookItem[name *= 'foo']" "BookItem[name *= \"foo\"]"
  expectSerialized "BookItem[name ^= 'foo']" "BookItem[name ^= \"foo\"]"
  expectSerialized "BookItem[name $= 'foo']" "BookItem[name $= \"foo\"]"
  expectSerialized "BookItem[name ~= 'foo']" "BookItem[name ~= \"foo\"]"
  expectSerialized "BookItem[name |= 'foo']" "BookItem[name |= \"foo\"]"

testToleratesSpacing :: Assertion
testToleratesSpacing = do
  expectSerialized " BookItem[ name = \"Foo \"  ]" "BookItem[name = \"Foo \"]"
  expectSerialized "  BookItem  [  name = \"Foo\"  ]   " "BookItem[name = \"Foo\"]"
  expectSerialized "  [    name = \"Foo\"]" "[name = \"Foo\"]"
  expectSerialized "  BookItem  [\"name\" = \"Foo\" i]   " "BookItem[name = \"Foo\" i]"
  expectSerialized "BookItem  [   'name' = 'Foo'i ]   " "BookItem[name = \"Foo\" i]"

testEscape :: Assertion
testEscape = do
  expectSerialized "BookItem['jake\\'s' = 10]" "BookItem[\"jake's\" = 10]"
  expectSerialized "BookItem['jake\"s' = 10]" "BookItem[\"jake\\\"s\" = 10]"
  expectSerialized "BookItem[\"jake\\\"s\" = 10]" "BookItem[\"jake\\\"s\" = 10]"
  expectSerialized "BookItem[name = 'foo\\'bar']" "BookItem[name = \"foo'bar\"]"
  expectSerialized "BookItem[name = \"foo'bar\"]" "BookItem[name = \"foo'bar\"]"
  expectSerialized "BookItem[name = \"foo\\\"bar\"]" "BookItem[name = \"foo\\\"bar\"]"

testParseIntValues :: Assertion
testParseIntValues = do
  expectSerialized "ColorButton[value = 10]" "ColorButton[value = 10]"
  expectSerialized "ColorButton[value = +10]" "ColorButton[value = 10]"
  expectSerialized "ColorButton[value = -10]" "ColorButton[value = -10]"
  expectSerialized "ColorButton  [ \"nested\". \"index\" = 0 ]   " "ColorButton[nested.index = 0]"

testParseFloatValues :: Assertion
testParseFloatValues = do
  expectSerialized "ColorButton[value = -12.3]" "ColorButton[value = -12.3]"
  expectSerialized "ColorButton  ['nested'.value = 4.1]" "ColorButton[nested.value = 4.1]"
  expectSerialized "ColorButton  [    'nested' .value =4.1]" "ColorButton[nested.value = 4.1]"

testParseBool :: Assertion
testParseBool = do
  expectSerialized "ColorButton[enabled= false] " "ColorButton[enabled = false]"
  expectSerialized "ColorButton[enabled  =true] " "ColorButton[enabled = true]"
  expectSerialized "ColorButton[enabled  =true][ color = \"red\"]" "ColorButton[enabled = true][color = \"red\"]"
  expectSerialized
    "ColorButton[ enabled  =true][ color = \"red\"i][nested.index =  6]"
    "ColorButton[enabled = true][color = \"red\" i][nested.index = 6]"

testParseRegex :: Assertion
testParseRegex = do
  expectSerialized "ColorButton[color =  /red$/]" "ColorButton[color = /red$/]"
  expectSerialized "ColorButton[color=/red/ig]" "ColorButton[color = /red/gi]"
  expectSerialized "ColorButton[color=  / \\/ [/]/  ]" "ColorButton[color = / \\/ [/]/]"
  expectSerialized "ColorButton[color=/[\\]/][[/]/]" "ColorButton[color = /[\\]/][[/]/]"

testParseIdentifiers :: Assertion
testParseIdentifiers = do
  expectSerialized "[привет=true]" "[\"привет\" = true]"
  expectSerialized "[__-__=true]" "[\"__-__\" = true]"
  expectSerialized "[😀=true]" "[\"😀\" = true]"

testParseUnquotedString :: Assertion
testParseUnquotedString = do
  expectSerializedUnquoted "[hey=foo]" "[hey = \"foo\"]"
  expectSerializedUnquoted "[yay=and😀more]" "[yay = \"and😀more\"]"
  expectSerializedUnquoted "[yay= trims  ]" "[yay = \"trims\"]"

testThrowOnMalformed :: Assertion
testThrowOnMalformed = do
  expectError "foo["
  expectError "foo[\"asd"
  expectError "foo[\"asd\""

  expectError "foo[.bar=10]"
  expectError "foo[bar **= 10]"
  expectError "foo[bar == 10]"
  expectError "foo[bar = 10 [baz=20]"
  expectError "foo[bar = 10 i[baz=20]"

  expectError "foo[bar *= #%s]"
  expectError "foo[bar *= 10]"
  expectError ""

  expectError "[foo=10 s]"
  expectError "[foo=10 p]"
  expectError "foo.bar"
  expectError "foo[]"
  expectError "[\"a\"b\"=foo]"
  expectError "[foo=10\"bar\"]"
  expectError "[foo= ==]"
  expectError "[foo===]"
  expectError "[foo=\"\\\"]\"[]"
  expectError "[foo=abc S]"
  expectError "[foo=abc s]"
  expectError "[foo=abc\"s\"]"
  expectError "[foo=\"\\\"]"
  expectError "[foo s]"
  expectError "[foo*=/bar/]"
  expectError "[foo=/bar/ s]"
  expectError "[foo=/bar//]"
  expectError "[foo=/bar/pt]"
  expectError "[foo=/[\\]/"

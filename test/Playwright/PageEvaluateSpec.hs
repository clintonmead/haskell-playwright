module Playwright.PageEvaluateSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright
  ( SpecialJsValue (SpecialInfinity, SpecialNaN, SpecialNegInfinity, SpecialNegZero, SpecialNull, SpecialUndefined)
  , boolArgument
  , evaluate
  , evaluateBoolWith
  , evaluateHandleExpression
  , jsonValueNumber
  , urlToText
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateSpecial
  , evaluateSpecialWith
  , evaluateTextWith
  , evaluateWith
  , numberArgument
  , numbersArgument
  , objectNumberArgument
  , objectTextArgument
  , specialArgument
  , textArgument
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssetPage
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadBigInt, PayloadDate, PayloadObject, PayloadString, PayloadUrl)
  , SerializedValue (SerializedNode)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

tests :: TestTree
tests =
  testGroup
    "page-evaluate"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          result <- evaluateNumber page "() => 7 * 3"
          assertEqual "21" (21 :: Double) result
    , testCase "should transfer NaN" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNaN)
          assertEqual "NaN" SpecialNaN special
    , testCase "should transfer -0" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNegZero)
          assertEqual "-0" SpecialNegZero special
    , testCase "should transfer Infinity" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialInfinity)
          assertEqual "Infinity" SpecialInfinity special
    , testCase "should transfer -Infinity" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNegInfinity)
          assertEqual "-Infinity" SpecialNegInfinity special
    , testCase "should transfer null" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialNull)
          assertEqual "null" SpecialNull special
    , testCase "should transfer undefined" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "a => a" (specialArgument SpecialUndefined)
          assertEqual "undefined" SpecialUndefined special
    , testCase "should transfer 0" $
        withChromiumPage $ \page -> do
          result <- evaluateNumberWith page "a => a" (numberArgument 0)
          assertEqual "0" (0 :: Double) result
    , testCase "should transfer 100" $
        withChromiumPage $ \page -> do
          result <- evaluateNumberWith page "a => a" (numberArgument 100)
          assertEqual "100" (100 :: Double) result
    , testCase "should transfer -1" $
        withChromiumPage $ \page -> do
          result <- evaluateNumberWith page "a => a" (numberArgument (-1))
          assertEqual "-1" ((-1) :: Double) result
    , testCase "should transfer true" $
        withChromiumPage $ \page -> do
          result <- evaluateBoolWith page "a => a" (boolArgument True)
          assertEqual "true" True result
    , testCase "should transfer false" $
        withChromiumPage $ \page -> do
          result <- evaluateBoolWith page "a => a" (boolArgument False)
          assertEqual "false" False result
    , testCase "should transfer string" $
        withChromiumPage $ \page -> do
          result <- evaluateTextWith page "a => a" (textArgument "hello")
          assertEqual "string" ("hello" :: Text) result
    , testCase "should transfer unicode string" $
        withChromiumPage $ \page -> do
          result <- evaluateTextWith page "a => a" (textArgument "こんにちは")
          assertEqual "unicode" ("こんにちは" :: Text) result
    , testCase "should return undefined for empty function" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => {}"
          assertEqual "undefined" SpecialUndefined special
    , testCase "should transfer arrays" $
        withChromiumPage $ \page -> do
          result <- evaluateNumbersWith page "a => a" (numbersArgument [1, 2, 3])
          assertEqual "array" [1, 2, 3 :: Double] result
    , testCase "should transfer arrays as arrays, not objects" $
        withChromiumPage $ \page -> do
          result <- evaluateBoolWith page "a => Array.isArray(a)" (numbersArgument [1, 2, 3])
          assertEqual "isArray" True result
    , testCase "should modify global environment" $
        withChromiumPage $ \page -> do
          _ <- evaluateNumber page "() => { window.globalVar = 123; return 123; }"
          result <- evaluateNumber page "() => window.globalVar"
          assertEqual "global" (123 :: Double) result
    , testCase "should evaluate in the page context" $
        withAssetPage "global-var.html" $ \page -> do
          result <- evaluateNumber page "() => window.globalVar"
          assertEqual "global" (123 :: Double) result
    , testCase "should work with unicode chars" $
        withChromiumPage $ \page -> do
          result <-
            evaluateNumberWith
              page
              "a => a['中文字符']"
              (objectNumberArgument [("中文字符", 42)])
          assertEqual "unicode field" (42 :: Double) result
    , testCase "should work with large strings" $
        withChromiumPage $ \page -> do
          let expected :: Text
              expected = Text.replicate 40000 "x"
          result <- evaluateTextWith page "data => data" (textArgument expected)
          assertEqual "large" expected result
    , testCase "should await promise" $
        withChromiumPage $ \page -> do
          result <- evaluateNumber page "() => Promise.resolve(8 * 7)"
          assertEqual "56" (56 :: Double) result
    , testCase "should reject promise with exception" $
        withChromiumPage $ \page ->
          assertProtocolContains "not_existing_object" (evaluateNumber page "() => not_existing_object.property")
    , testCase "should support thrown strings as error messages" $
        withChromiumPage $ \page ->
          assertProtocolContains "qwerty" (evaluateNumber page "() => { throw 'qwerty'; }")
    , testCase "should support thrown numbers as error messages" $
        withChromiumPage $ \page ->
          assertProtocolContains "100500" (evaluateNumber page "() => { throw 100500; }")
    , testCase "should return complex objects" $
        withChromiumPage $ \page -> do
          result <- evaluateWith page "a => a" (objectTextArgument [("foo", "bar!")])
          case result of
            SerializedNode (PayloadObject fields) _ ->
              case lookup ("foo" :: Text) fields of
                Just (SerializedNode (PayloadString text) _) ->
                  assertEqual "foo" ("bar!" :: Text) text
                _ ->
                  assertFailure "missing foo"
            _ ->
              assertFailure "expected object"
    , testCase "should work with large unicode strings" $
        withChromiumPage $ \page -> do
          let expected :: Text
              expected = Text.replicate 10000 "🎭"
          result <- evaluateTextWith page "data => data" (textArgument expected)
          assertEqual "unicode" expected result
    , testCase "should transfer bigint" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => 42n"
          case result of
            SerializedNode (PayloadBigInt digits) _ ->
              assertEqual "bigint" ("42" :: Text) digits
            _ ->
              assertFailure "expected bigint"
    , testCase "should return NaN" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => NaN"
          assertEqual "NaN" SpecialNaN special
    , testCase "should return -0" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => -0"
          assertEqual "-0" SpecialNegZero special
    , testCase "should return Infinity" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => Infinity"
          assertEqual "Infinity" SpecialInfinity special
    , testCase "should return -Infinity" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => -Infinity"
          assertEqual "-Infinity" SpecialNegInfinity special
    , testCase "should roundtrip promise to value" $
        withChromiumPage $ \page -> do
          mapM_
            ( \special -> do
                result <- evaluateSpecialWith page "value => Promise.resolve(value)" (specialArgument special)
                assertEqual (show special) special result
            )
            [SpecialNull, SpecialInfinity, SpecialNegZero, SpecialUndefined]
    , testCase "should evaluate date" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => new Date('2020-02-05T00:00:00.000Z')"
          case result of
            SerializedNode (PayloadDate iso) _ ->
              assertEqual "date" "2020-02-05T00:00:00.000Z" iso
            _ ->
              assertFailure ("expected a date, got " <> show result)
    , testCase "should evaluate url" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => new URL('https://example.com/')"
          case result of
            SerializedNode (PayloadUrl url) _ ->
              assertEqual "url" "https://example.com/" (urlToText url)
            _ ->
              assertFailure ("expected a url, got " <> show result)
    , testCase "should accept a string" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandleExpression page "1 + 2"
          assertEqual "sum" (3 :: Double) =<< jsonValueNumber handle
    , testCase "should accept a string with semi colons" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandleExpression page "2 + 5;"
          assertEqual "sum" (7 :: Double) =<< jsonValueNumber handle
    , testCase "should accept a string with comments" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandleExpression page "2 + 5;\n// do some stuff"
          assertEqual "sum" (7 :: Double) =<< jsonValueNumber handle
    , testCase "should evaluate exception" $
        withChromiumPage $ \page ->
          assertProtocolContains "Bar" (evaluate page "() => { throw new Error('Bar'); }")

    ]

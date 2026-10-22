module Playwright.PageEvaluateSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , replace
  )
import Data.Text qualified as Text
import Data.Maybe
  ( fromMaybe
  )
import UnliftIO.Async
  ( async
  , wait
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , requestHeaderHost
  , responseLBS
  )
import Playwright
  ( SpecialJsValue (SpecialInfinity, SpecialNaN, SpecialNegInfinity, SpecialNegZero, SpecialNull, SpecialUndefined)
  , boolArgument
  , evaluate
  , evaluateBool
  , evaluateBoolWith
  , evaluateHandleExpression
  , elementArgument
  , jsonValue
  , jsonValueNumber
  , urlFromText
  , urlToText
  , evaluateNumber
  , evaluateNumberWith
  , evaluateNumbersWith
  , evaluateOnFrame
  , evaluateSpecial
  , evaluateSpecialWith
  , evaluateTextWith
  , evaluateWith
  , defaultGotoOptions
  , goto
  , numberArgument
  , numbersArgument
  , objectNumberArgument
  , objectSpecialArgument
  , objectTextArgument
  , onFrameNavigated
  , querySelector
  , selectorFromText
  , setContent
  , specialArgument
  , textArgument
  , Playwright
  )
import Playwright.Harness
  ( assertProtocolContains
  , requireHandle
  , withAssetPage
  , withAssets
  , withChromiumPage
  , withLocalApp
  , assertBool
  , assertEqual
  , assertFailure
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadBigInt, PayloadDate, PayloadNumber, PayloadObject, PayloadSpecial, PayloadString, PayloadUrl)
  , SerializedValue (SerializedNode, SerializedRef)
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
    , testCase "should roundtrip unserializable values" $
        withChromiumPage $ \page -> do
          result <- evaluateWith page "value => value" unserializableFields
          assertUnserializableFields result
    , testCase "should roundtrip promise to unserializable values" $
        withChromiumPage $ \page -> do
          result <- evaluateWith page "value => Promise.resolve(value)" unserializableFields
          assertUnserializableFields result
    , testCase "should accept element handle as an argument" $
        withChromiumPage $ \page -> do
          setContent page "<section>42</section>"
          handle <- requireHandle =<< querySelector page (selectorFromText "section")
          result <- evaluateTextWith page "e => e.textContent" (elementArgument handle)
          assertEqual "text" ("42" :: Text) result
    , testCase "should jsonValue() date" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandleExpression page "new Date('2020-02-05T00:00:00.000Z')"
          result <- jsonValue handle
          case result of
            SerializedNode (PayloadDate iso) _ ->
              assertEqual "date" "2020-02-05T00:00:00.000Z" iso
            _ ->
              assertFailure ("expected a date, got " <> show result)
    , testCase "should jsonValue() url" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandleExpression page "new URL('https://example.com/')"
          result <- jsonValue handle
          case result of
            SerializedNode (PayloadUrl url) _ ->
              assertEqual "url" "https://example.com/" (urlToText url)
            _ ->
              assertFailure ("expected a url, got " <> show result)
    , testCase "should evaluate date nested in an object" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => ({ d: new Date('2020-02-05T00:00:00.000Z') })"
          case result of
            SerializedNode (PayloadObject fields) _ ->
              case lookup ("d" :: Text) fields of
                Just (SerializedNode (PayloadDate iso) _) ->
                  assertEqual "date" "2020-02-05T00:00:00.000Z" iso
                _ ->
                  assertFailure "missing date"
            _ ->
              assertFailure "expected object"
    , testCase "should evaluate url nested in an object" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => ({ u: new URL('https://example.com/') })"
          case result of
            SerializedNode (PayloadObject fields) _ ->
              case lookup ("u" :: Text) fields of
                Just (SerializedNode (PayloadUrl url) _) ->
                  assertEqual "url" "https://example.com/" (urlToText url)
                _ ->
                  assertFailure "missing url"
            _ ->
              assertFailure "expected object"
    , testCase "should not use toJSON when evaluating" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => ({ toJSON: () => 'string', data: 'data' })"
          case result of
            SerializedNode (PayloadObject fields) _ -> do
              assertEqual "data" (Just ("data" :: Text)) (textFieldOf "data" fields)
              assertEqual "toJSON" Nothing (textFieldOf "toJSON" fields)
            _ ->
              assertFailure "expected object"
    , testCase "should work for circular object" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => { const a = {}; a.b = a; return a; }"
          assertBool "circular" (isSelfCycle result)
    , testCase "should throw for too deep reference chain" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "object reference chain is too long"
            (evaluate page "() => { const obj = {}; let temp = obj; for (let i = 0; i < 1000; i++) { temp[i] = {}; temp = temp[i]; } return obj; }")
    , testCase "should throw for too deep reference chain 2" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "object reference chain is too long"
            (evaluate page "() => { let node = {}; for (let i = 0; i < 200; i++) node = { child: node }; return node; }")
    , testCase "should throw when evaluation triggers reload" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "navigation"
            (evaluate page "() => { location.reload(); return new Promise(() => {}); }")
    , testCase "should not throw an error when evaluation does a navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          result <-
            evaluateNumbersWith
              page
              "() => { window.location.href = '/empty.html'; return [42]; }"
              (specialArgument SpecialUndefined)
          assertEqual "result" [42] result
    , testCase "should not throw an error when evaluation does a synchronous navigation and returns an object" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => { window.location.reload(); return { a: 42 }; }"
          assertEqual "a" (Just 42) (objectNumberField "a" result)
    , testCase "should not throw an error when evaluation does a synchronous navigation and returns undefined" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecial page "() => { window.location.reload(); return undefined; }"
          assertEqual "undefined" SpecialUndefined special
    , testCase "should work with new Function() and CSP" $
        withChromiumPage $ \page ->
          withLocalApp scriptHostApp $ \base -> do
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            assertEqual "result" True =<< evaluateBool page "() => new Function('return true')()"
    , testCase "should work with CSP" $
        withChromiumPage $ \page ->
          withLocalApp scriptSelfApp $ \base -> do
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            assertEqual "four" (4 :: Double) =<< evaluateNumber page "() => 2 + 2"
    , testCase "should work right after framenavigated" $
        withAssets $ \prefix page -> do
          evaluation <- newEmptyMVar
          onFrameNavigated page $ \frame -> do
            pending <- async (evaluateOnFrame frame "() => 6 * 7")
            _ <- tryPutMVar evaluation pending
            pure ()
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          pending <- takeMVar evaluation
          assertEqual "result" (Just 42) . numberOf =<< wait pending
    , testCase "should work right after a cross-origin navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          evaluation <- newEmptyMVar
          onFrameNavigated page $ \frame -> do
            pending <- async (evaluateOnFrame frame "() => 6 * 7")
            _ <- tryPutMVar evaluation pending
            pure ()
          -- localhost is a different origin from 127.0.0.1, and the same server answers both.
          _ <- goto page (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/empty.html")) defaultGotoOptions
          pending <- takeMVar evaluation
          assertEqual "result" (Just 42) . numberOf =<< wait pending
    ]

-- | The number a frame evaluation returned, or 'Nothing' when it is not a number.
numberOf :: SerializedValue -> Maybe Double
numberOf value =
  case value of
    SerializedNode (PayloadNumber number) _ -> Just number
    _ -> Nothing

-- | Serves an empty page whose Content-Security-Policy allows scripts from the
-- page's own origin only, the header the upstream "should work with CSP" case sets.
scriptSelfApp :: Application
scriptSelfApp _ respond =
  respond
    ( responseLBS
        status200
        [ ("Content-Security-Policy", "script-src 'self'")
        , (hContentType, "text/html")
        ]
        ""
    )

-- | Serves an empty page whose Content-Security-Policy allows scripts from the
-- host the request was sent to. The app is built before the test knows its base
-- URL, so it reads the @Host@ header, as the upstream case reads its server prefix.
scriptHostApp :: Application
scriptHostApp request respond =
  respond
    ( responseLBS
        status200
        [ ("Content-Security-Policy", "script-src http://" <> fromMaybe "" (requestHeaderHost request))
        , (hContentType, "text/html")
        ]
        ""
    )

-- | The number stored under one field of an object result, or 'Nothing' when the
-- value is not an object or the field is not a number.
objectNumberField :: Text -> SerializedValue -> Maybe Double
objectNumberField name value =
  case value of
    SerializedNode (PayloadObject fields) _ ->
      case lookup name fields of
        Just (SerializedNode (PayloadNumber number) _) ->
          Just number
        _ ->
          Nothing
    _ ->
      Nothing

-- | True when the value is an object whose @b@ field refers back to the object
-- itself, which is how the driver encodes @const a = {}; a.b = a@.
isSelfCycle :: SerializedValue -> Bool
isSelfCycle value =
  case value of
    SerializedNode (PayloadObject [("b", SerializedRef target)]) (Just self) ->
      target == self
    _ ->
      False

unserializableFields :: SerializedArgument
unserializableFields =
  objectSpecialArgument
    [ ("infinity", SpecialInfinity)
    , ("nInfinity", SpecialNegInfinity)
    , ("nZero", SpecialNegZero)
    , ("nan", SpecialNaN)
    ]

assertUnserializableFields :: SerializedValue -> Playwright ()
assertUnserializableFields result =
  case result of
    SerializedNode (PayloadObject fields) _ -> do
      assertEqual "infinity" (Just SpecialInfinity) (specialFieldOf "infinity" fields)
      assertEqual "nInfinity" (Just SpecialNegInfinity) (specialFieldOf "nInfinity" fields)
      assertEqual "nZero" (Just SpecialNegZero) (specialFieldOf "nZero" fields)
      assertEqual "nan" (Just SpecialNaN) (specialFieldOf "nan" fields)
    _ ->
      assertFailure "expected object"

specialFieldOf :: Text -> [(Text, SerializedValue)] -> Maybe SpecialJsValue
specialFieldOf name fields =
  case lookup name fields of
    Just (SerializedNode (PayloadSpecial special) _) -> Just special
    _ -> Nothing

textFieldOf :: Text -> [(Text, SerializedValue)] -> Maybe Text
textFieldOf name fields =
  case lookup name fields of
    Just (SerializedNode (PayloadString text) _) -> Just text
    _ -> Nothing

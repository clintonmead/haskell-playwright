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
  , isJust
  )
import Data.ByteString.Builder
  ( Builder
  , doubleLE
  , floatLE
  , int16LE
  , int32LE
  , int64LE
  , int8
  , toLazyByteString
  , word16LE
  , word32LE
  , word64LE
  , word8
  )
import Data.ByteString.Lazy qualified as LazyByteString
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
  , BindingName (BindingName)
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
  , evaluateTexts
  , evaluateWith
  , exposeFunction
  , defaultGotoOptions
  , goto
  , numberArgument
  , numbersArgument
  , objectArgument
  , objectNumberArgument
  , objectSpecialArgument
  , objectTextArgument
  , onFrameNavigated
  , querySelector
  , selectorFromText
  , setContent
  , specialArgument
  , textArgument
  , undefinedArgument
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
import Playwright.JsErrorValue
  ( JsErrorValue (JsErrorValue, jsErrorMessage, jsErrorName, jsErrorStack)
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryFromByteString
  , protocolBinaryToByteString
  )
import Playwright.SerializedArgument
  ( SerializedArgument (SerializedArgument, serializedArgumentHandles, serializedArgumentValue)
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadArray, PayloadBigInt, PayloadBool, PayloadDate, PayloadError, PayloadNumber, PayloadObject, PayloadRegex, PayloadSpecial, PayloadString, PayloadTypedArray, PayloadUrl)
  , SerializedValue (SerializedNode, SerializedRef)
  )
import Playwright.TypedArrayKind
  ( TypedArrayKind (TypedArrayBi64, TypedArrayBui64, TypedArrayF32, TypedArrayF64, TypedArrayI16, TypedArrayI32, TypedArrayI8, TypedArrayUi16, TypedArrayUi32, TypedArrayUi8, TypedArrayUi8c)
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
    , testCase "should work from-inside an exposed function" $
        withChromiumPage $ \page -> do
          _ <-
            exposeFunction page (BindingName "callController") $ \args ->
              case args of
                [SerializedNode (PayloadNumber a) _, SerializedNode (PayloadNumber b) _] -> do
                  times <- evaluateNumberWith page "({ a, b }) => a * b" (objectNumberArgument [("a", a), ("b", b)])
                  pure (numberArgument times)
                _ ->
                  pure (specialArgument SpecialUndefined)
          result <- evaluateNumber page "async () => await window['callController'](9, 3)"
          assertEqual "27" (27 :: Double) result
    , testCase "should alias Window, Document and Node" $
        withChromiumPage $ \page -> do
          aliases <- evaluateTexts page "() => [window, document, document.body]"
          assertEqual "aliases" ["ref: <Window>", "ref: <Document>", "ref: <Node>"] aliases
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
    , testCase "should work with function shorthands" $
        withChromiumPage $ \page -> do
          assertEqual "sum" (3 :: Double) =<< evaluateNumberWith page "args => ({ sum([a, b]) { return a + b; } }).sum(args)" (numbersArgument [1, 2])
          assertEqual "mult" (8 :: Double) =<< evaluateNumberWith page "args => ({ async mult([a, b]) { return a * b; } }).mult(args)" (numbersArgument [2, 4])
    , testCase "should throw error with detailed information on exception inside promise" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "Error in promise"
            (evaluate page "() => new Promise(() => { throw new Error('Error in promise'); })")
    , testCase "should work even when JSON is set to null" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { window.JSON.stringify = null; window.JSON = null; }"
          result <- evaluate page "() => ({ abc: 123 })"
          assertEqual "abc" (Just (123 :: Double)) (objectNumberField "abc" result)
    , testCase "should work with non-strict expressions" $
        withChromiumPage $ \page ->
          assertEqual "y" (3.14 :: Double) =<< evaluateNumber page "() => { y = 3.14; return y; }"
    , testCase "should respect use strict expression" $
        withChromiumPage $ \page ->
          assertProtocolContains
            "variableY"
            (evaluate page "() => { 'use strict'; variableY = 3.14; return variableY; }")
    , testCase "should not leak utility script" $
        withChromiumPage $ \page ->
          assertBool "this === window" =<< evaluateBool page "() => this === window"
    , testCase "should not leak handles" $
        withChromiumPage $ \page ->
          assertProtocolContains "handles is not defined" (evaluate page "() => handles.length")
    , testCase "should evaluate exception with a function on the stack" $
        withChromiumPage $ \page -> do
          result <-
            evaluate page "() => (function functionOnStack() { return new Error('error message'); })()"
          case result of
            SerializedNode (PayloadError err) _ -> do
              assertEqual "message" "error message" (jsErrorMessage err)
              assertBool "stack" (Text.isInfixOf "functionOnStack" (jsErrorStack err))
            _ ->
              assertFailure ("expected an error, got " <> show result)
    , testCase "should pass exception argument" $
        withChromiumPage $ \page -> do
          let argument =
                errorArgument
                  JsErrorValue
                    { jsErrorMessage = "error message"
                    , jsErrorName = "foobar"
                    , jsErrorStack = "Error: error message\n    at innerFunction"
                    }
          result <- evaluateWith page "e => ({ message: e.message, name: e.name, stack: e.stack })" argument
          case result of
            SerializedNode (PayloadObject fields) _ -> do
              assertEqual "message" (Just "error message") (textFieldOf "message" fields)
              assertEqual "name" (Just "foobar") (textFieldOf "name" fields)
              assertBool "stack" (maybe False (Text.isInfixOf "innerFunction") (textFieldOf "stack" fields))
            _ ->
              assertFailure ("expected an object, got " <> show result)
    , testCase "should work with overwritten Promise" $
        withChromiumPage $ \page -> do
          _ <-
            evaluate
              page
              "() => { const originalPromise = window.Promise; class Promise2 { static all(arg) { return wrap(originalPromise.all(arg)); } static race(arg) { return wrap(originalPromise.race(arg)); } static resolve(arg) { return wrap(originalPromise.resolve(arg)); } constructor(f) { this._promise = new originalPromise(f); } then(f, r) { return wrap(this._promise.then(f, r)); } catch(f) { return wrap(this._promise.catch(f)); } finally(f) { return wrap(this._promise.finally(f)); } }; const wrap = p => { const result = new Promise2(() => { }); result._promise = p; return result; }; window.Promise = Promise2; window['__Promise2'] = Promise2; }"
          assertBool "instanceof Promise2" =<< evaluateBool page "() => { const p = Promise.all([Promise.race([]), new Promise(() => { }).then(() => { })]); return p instanceof window['__Promise2']; }"
          assertEqual "resolved" (42 :: Double) =<< evaluateNumber page "() => Promise.resolve(42)"
    , testCase "should simulate a user gesture" $
        withChromiumPage $ \page ->
          assertBool "copy" =<< evaluateBool page "() => { document.body.appendChild(document.createTextNode('test')); document.execCommand('selectAll'); return document.execCommand('copy'); }"
    , testCase "should await promise from popup" $
        withChromiumPage $ \page ->
          assertEqual "popup promise" (42 :: Double) =<< evaluateNumber page "() => { const win = window.open('about:blank'); return new win['Promise'](f => f(42)); }"
    , testCase "should roundtrip unserializable values" $
        withChromiumPage $ \page -> do
          result <- evaluateWith page "value => value" unserializableFields
          assertUnserializableFields result
    , testCase "should roundtrip promise to unserializable values" $
        withChromiumPage $ \page -> do
          result <- evaluateWith page "value => Promise.resolve(value)" unserializableFields
          assertUnserializableFields result
    , testCase "should transfer typed arrays" $
        withChromiumPage $ \page ->
          mapM_
            ( \(kind, payload) -> do
                result <- evaluateWith page "a => a" (typedArrayArgument kind payload)
                let expected = LazyByteString.toStrict (toLazyByteString payload)
                case result of
                  SerializedNode (PayloadTypedArray bytes resultKind) _ -> do
                    assertEqual (show kind <> " kind") kind resultKind
                    assertEqual (show kind <> " bytes") expected (protocolBinaryToByteString bytes)
                  _ ->
                    assertFailure ("expected a typed array, got " <> show result)
            )
            [ (TypedArrayI8, foldMap int8 [1, 2, 3])
            , (TypedArrayUi8, foldMap word8 [1, 2, 3])
            , (TypedArrayUi8c, foldMap word8 [1, 2, 3])
            , (TypedArrayI16, foldMap int16LE [1, 2, 3])
            , (TypedArrayUi16, foldMap word16LE [1, 2, 3])
            , (TypedArrayI32, foldMap int32LE [1, 2, 3])
            , (TypedArrayUi32, foldMap word32LE [1, 2, 3])
            , (TypedArrayF32, foldMap floatLE [1.1, 2.2, 3.3])
            , (TypedArrayF64, foldMap doubleLE [1.1, 2.2, 3.3])
            , (TypedArrayBi64, foldMap int64LE [1, 2, 3])
            , (TypedArrayBui64, foldMap word64LE [1, 2, 3])
            ]
    , testCase "should properly serialize undefined arguments" $
        withChromiumPage $ \page -> do
          result <- evaluateWith page "x => ({ a: x })" undefinedArgument
          case result of
            SerializedNode (PayloadObject fields) _ ->
              assertBool "a is absent or undefined" (fieldUndefined "a" fields)
            _ ->
              assertFailure ("expected object, got " <> show result)
    , testCase "should properly serialize undefined fields" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => ({ a: undefined })"
          case result of
            SerializedNode (PayloadObject fields) _ ->
              assertBool "a is absent or undefined" (fieldUndefined "a" fields)
            _ ->
              assertFailure ("expected object, got " <> show result)
    , testCase "should return undefined properties" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => ({ a: undefined })"
          case result of
            SerializedNode (PayloadObject fields) _ -> do
              assertBool "a in value" (isJust (lookup "a" fields))
              assertEqual "a" (Just SpecialUndefined) (specialFieldOf "a" fields)
            _ ->
              assertFailure ("expected object, got " <> show result)
    , testCase "should properly serialize null arguments" $
        withChromiumPage $ \page -> do
          special <- evaluateSpecialWith page "x => x" (specialArgument SpecialNull)
          assertEqual "null" SpecialNull special
    , testCase "should properly serialize null fields" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => ({ a: null })"
          case result of
            SerializedNode (PayloadObject fields) _ ->
              assertEqual "a" (Just SpecialNull) (specialFieldOf "a" fields)
            _ ->
              assertFailure ("expected object, got " <> show result)
    , testCase "should roundtrip date" $
        withChromiumPage $ \page -> do
          let date = SerializedNode (PayloadDate "2020-05-27T01:31:38.506Z") Nothing
          result <- evaluateWith page "date => date" (SerializedArgument date [])
          case result of
            SerializedNode (PayloadDate iso) _ ->
              assertEqual "date" "2020-05-27T01:31:38.506Z" iso
            _ ->
              assertFailure ("expected a date, got " <> show result)
    , testCase "should roundtrip regex" $
        withChromiumPage $ \page -> do
          let regex = SerializedNode (PayloadRegex "hello" "im") Nothing
          result <- evaluateWith page "regex => regex" (SerializedArgument regex [])
          case result of
            SerializedNode (PayloadRegex pattern flags) _ -> do
              assertEqual "pattern" "hello" pattern
              assertEqual "flags" "im" flags
            _ ->
              assertFailure ("expected a regex, got " <> show result)
    , testCase "should roundtrip url" $
        withChromiumPage $ \page -> do
          let url = SerializedNode (PayloadUrl (urlFromText "https://example.com/")) Nothing
          result <- evaluateWith page "url => url" (SerializedArgument url [])
          case result of
            SerializedNode (PayloadUrl resultUrl) _ ->
              assertEqual "url" "https://example.com/" (urlToText resultUrl)
            _ ->
              assertFailure ("expected a url, got " <> show result)
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
    , testCase "should throw a nice error after a navigation" $
        withChromiumPage $ \page -> do
          pending <- async (evaluate page "() => new Promise(f => window.__resolve = f)")
          -- The page sets __resolve when the pending evaluate runs, so this is the barrier
          -- that keeps the reload from overtaking it.
          let waitForResolver :: Int -> Playwright ()
              waitForResolver 0 = assertFailure "the pending evaluate never reached the page"
              waitForResolver remaining = do
                ready <- evaluateBool page "() => typeof window.__resolve === 'function'"
                if ready
                  then pure ()
                  else waitForResolver (remaining - 1)
          waitForResolver 100
          _ <- evaluate page "() => { window.location.reload(); setTimeout(() => window.__resolve(42), 1000); }"
          assertProtocolContains "navigation" (wait pending)
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
    , testCase "should return undefined for non-serializable objects" $
        withChromiumPage $ \page ->
          assertEqual "undefined" SpecialUndefined =<< evaluateSpecial page "() => function() {}"
    , testCase "should not use Array.prototype.toJSON when evaluating" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => { Array.prototype.toJSON = () => 'busted'; return [1, 2, 3]; }"
          case result of
            SerializedNode (PayloadArray items) _ ->
              assertEqual "items" [Just 1, Just 2, Just 3] (map numberOf items)
            _ ->
              assertFailure ("expected array, got " <> show result)
    , testCase "should not add a toJSON property to newly created Arrays after evaluation" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => []"
          assertEqual "toJSON" False =<< evaluateBool page "() => 'toJSON' in []"
    , testCase "should not use toJSON in jsonValue" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandleExpression page "({ toJSON: () => 'string', data: 'data' })"
          result <- jsonValue handle
          case result of
            SerializedNode (PayloadObject fields) _ -> do
              assertEqual "data" (Just ("data" :: Text)) (textFieldOf "data" fields)
              assertBool "toJSON is an empty object" (isEmptyObjectField "toJSON" fields)
            _ ->
              assertFailure ("expected object, got " <> show result)
    , testCase "should ignore buggy toJSON" $
        withChromiumPage $ \page -> do
          result <-
            evaluate
              page
              "() => { class Foo { toJSON() { throw new Error('Bad'); } } class Bar { get toJSON() { throw new Error('Also bad'); } } return { foo: new Foo(), bar: new Bar() }; }"
          case result of
            SerializedNode (PayloadObject fields) _ -> do
              assertBool "foo is an empty object" (isEmptyObjectField "foo" fields)
              assertBool "bar is an empty object" (isEmptyObjectField "bar" fields)
            _ ->
              assertFailure ("expected object, got " <> show result)
    , testCase "should not expose the injected script export" $
        withChromiumPage $ \page ->
          assertEqual "pwExport undefined" True =<< evaluateBool page "() => typeof pwExport === 'undefined'"
    , testCase "should work with Array.from/map" $
        withChromiumPage $ \page -> do
          result <-
            evaluateTextWith
              page
              "() => { const r = (str, amount) => Array.from(Array(amount)).map(() => str).join(''); return r('([a-f0-9]{2})', 3); }"
              undefinedArgument
          assertEqual "pattern" ("([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})" :: Text) result
    , testCase "should work with overridden Object.defineProperty" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<script>Object.create = null; Object.defineProperty = null; Object.getOwnPropertyDescriptor = null; Object.getOwnPropertyNames = null; Object.getPrototypeOf = null; Object.prototype.hasOwnProperty = null;</script>"
          assertEqual "sum" (3 :: Double) =<< evaluateNumber page "() => 1 + 2"
    , testCase "should work with busted Array.prototype.map/push" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<script>Array.prototype.map = null; Array.prototype.push = null;</script>"
          assertEqual "sum" (3 :: Double) =<< evaluateNumber page "() => 1 + 2"
          handle <- evaluateHandleExpression page "1+2"
          assertEqual "handle sum" (3 :: Double) =<< jsonValueNumber handle
    , testCase "should properly serialize window.performance object" $
        withChromiumPage $ \page -> do
          result <- evaluate page "() => performance"
          case result of
            SerializedNode (PayloadObject fields) _ -> do
              assertEqual "timeOrigin" True (isJust (lookup "timeOrigin" fields >>= numberOf))
              let navigation = case lookup "navigation" fields of
                    Just (SerializedNode (PayloadObject navFields) _) -> navFields
                    _ -> []
              assertEqual "redirectCount" (Just 0) (lookup "redirectCount" navigation >>= numberOf)
            _ -> assertFailure "expected window.performance to serialize as an object"
    , testCase "should ignore dangerous object keys" $
        withChromiumPage $ \page -> do
          result <-
            evaluateWith
              page
              "arg => arg"
              ( objectArgument
                  [ ("__proto__", SerializedNode (PayloadObject [("polluted", SerializedNode (PayloadBool True) Nothing)]) Nothing)
                  , ("safeKey", SerializedNode (PayloadString "safeValue") Nothing)
                  ]
              )
          case result of
            SerializedNode (PayloadObject fields) _ ->
              assertEqual "keys" ["safeKey" :: Text] (map fst fields)
            _ ->
              assertFailure ("expected object, got " <> show result)
    , testCase "should properly serialize PerformanceMeasure object" $
        withChromiumPage $ \page -> do
          result <-
            evaluate
              page
              "() => { performance.mark('start'); performance.mark('end'); performance.measure('my-measure', 'start', 'end'); return performance.getEntriesByType('measure'); }"
          case result of
            SerializedNode (PayloadArray [SerializedNode (PayloadObject fields) _]) _ -> do
              assertEqual "name" (Just ("my-measure" :: Text)) (textFieldOf "name" fields)
              assertEqual "entryType" (Just ("measure" :: Text)) (textFieldOf "entryType" fields)
            _ ->
              assertFailure ("expected one measure, got " <> show result)
    , testCase "should return undefined for objects with symbols" $
        withChromiumPage $ \page -> do
          symbolArray <- evaluate page "() => [Symbol('foo4')]"
          case symbolArray of
            SerializedNode (PayloadArray [SerializedNode (PayloadSpecial SpecialUndefined) _]) _ ->
              pure ()
            _ ->
              assertFailure ("expected [undefined], got " <> show symbolArray)
          symbolKeyed <- evaluate page "() => { const a = {}; a[Symbol('foo4')] = 42; return a; }"
          case symbolKeyed of
            SerializedNode (PayloadObject []) _ ->
              pure ()
            _ ->
              assertFailure ("expected {}, got " <> show symbolKeyed)
          nested <- evaluate page "() => ({ foo: [{ a: Symbol('foo4') }] })"
          case nested of
            SerializedNode (PayloadObject [("foo", SerializedNode (PayloadArray [SerializedNode (PayloadObject [("a", SerializedNode (PayloadSpecial SpecialUndefined) _)]) _]) _)]) _ ->
              pure ()
            _ ->
              assertFailure ("expected { foo: [{ a: undefined }] }, got " <> show nested)
    , testCase "should work with overridden globalThis.Window/Document/Node" $
        withChromiumPage $ \page ->
          mapM_
            ( \override -> do
                _ <- evaluate page override
                assertEqual "sum" (3 :: Double) =<< evaluateNumber page "() => 1 + 2"
                result <- evaluate page "() => ['foo']"
                case result of
                  SerializedNode (PayloadArray [SerializedNode (PayloadString "foo") _]) _ -> pure ()
                  _ -> assertFailure ("expected ['foo'], got " <> show result)
            )
            [ "() => globalThis.Window = {}"
            , "() => globalThis.Document = {}"
            , "() => globalThis.Node = {}"
            , "() => globalThis.Window = null"
            , "() => globalThis.Document = null"
            , "() => globalThis.Node = null"
            ]
    , testCase "should work with overridden URL/Date/RegExp" $
        withChromiumPage $ \page ->
          mapM_
            ( \override -> do
                _ <- evaluate page override
                assertEqual "sum" (3 :: Double) =<< evaluateNumber page "() => 1 + 2"
                result <- evaluate page "() => ({ 'a': 2023 })"
                case result of
                  SerializedNode (PayloadObject [("a", SerializedNode (PayloadNumber 2023) _)]) _ -> pure ()
                  _ -> assertFailure ("expected { a: 2023 }, got " <> show result)
            )
            [ "() => globalThis.URL = 'foo'"
            , "() => globalThis.RegExp = 'foo'"
            , "() => globalThis.Date = 'foo'"
            ]
    , testCase "should work with a using declaration" $
        withChromiumPage $ \page -> do
          disposed <-
            evaluateBool
              page
              "() => { let disposed = false; { using r = { [Symbol.dispose]: () => { disposed = true; } }; void r; } return disposed; }"
          assertEqual "disposed" True disposed
    ]

-- | True when the field is present and serialized as an object with no fields.
isEmptyObjectField :: Text -> [(Text, SerializedValue)] -> Bool
isEmptyObjectField name fields =
  case lookup name fields of
    Just (SerializedNode (PayloadObject []) _) -> True
    _ -> False

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

-- | True when an object field is either missing or serialized as @undefined@.
fieldUndefined :: Text -> [(Text, SerializedValue)] -> Bool
fieldUndefined name fields =
  case lookup name fields of
    Nothing -> True
    Just (SerializedNode (PayloadSpecial SpecialUndefined) _) -> True
    Just _ -> False

-- | An Error argument built from its serialized fields, with no handles. Haskell has no
-- way to create a page Error handle as an argument, so the caller supplies the stack.
errorArgument :: JsErrorValue -> SerializedArgument
errorArgument err =
  SerializedArgument
    { serializedArgumentValue = SerializedNode (PayloadError err) Nothing
    , serializedArgumentHandles = []
    }

-- | A typed-array argument built from its raw little-endian payload, with no handles.
typedArrayArgument :: TypedArrayKind -> Builder -> SerializedArgument
typedArrayArgument kind payload =
  SerializedArgument
    { serializedArgumentValue =
        SerializedNode
          (PayloadTypedArray (protocolBinaryFromByteString (LazyByteString.toStrict (toLazyByteString payload))) kind)
          Nothing
    , serializedArgumentHandles = []
    }

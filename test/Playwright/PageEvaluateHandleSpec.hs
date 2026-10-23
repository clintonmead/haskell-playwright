module Playwright.PageEvaluateHandleSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( JsHandle
  , arrayValue
  , evaluateBoolWith
  , evaluateHandle
  , evaluateNumberWith
  , evaluateTextWith
  , jsHandleArgument
  , numberValue
  , objectArgument
  , objectNumberArgument
  , objectValue
  )
import Playwright.Harness
  ( withChromiumPage
  , assertBool
  , assertEqual
  )
import Playwright.SerializedArgument
  ( SerializedArgument (SerializedArgument, serializedArgumentHandles, serializedArgumentValue)
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadHandle)
  , SerializedValue (SerializedNode)
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )

-- | Build an argument whose handles are passed in order. The builder receives one
-- handle node per entry in the list, so the same handle can appear several times.
argumentWithHandles :: [JsHandle] -> ([SerializedValue] -> SerializedValue) -> SerializedArgument
argumentWithHandles handles build =
  SerializedArgument
    { serializedArgumentValue = build (fmap (\index -> SerializedNode (PayloadHandle index) Nothing) [0 .. length handles - 1])
    , serializedArgumentHandles = concatMap (serializedArgumentHandles . jsHandleArgument) handles
    }

tests :: TestTree
tests =
  testGroup
    "page-evaluate-handle"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => window"
          assertEqual "window" True =<< evaluateBoolWith page "e => e === window" (jsHandleArgument handle)
    , testCase "should accept object handle as an argument" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => navigator"
          userAgent <- evaluateTextWith page "e => e.userAgent" (jsHandleArgument handle)
          assertBool "Mozilla" ("Mozilla" `isInfixOf` userAgent)
    , testCase "should accept object handle to primitive types" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => 5"
          assertEqual "is five" True =<< evaluateBoolWith page "e => Object.is(e, 5)" (jsHandleArgument handle)
    , testCase "should accept nested handle" $
        withChromiumPage $ \page -> do
          foo <- evaluateHandle page "() => ({ x: 1, y: 'foo' })"
          let arg = argumentWithHandles [foo] (\nodes -> objectValue [("foo", nodes !! 0)])
          result <- evaluateTextWith page "({ foo }) => JSON.stringify(foo)" arg
          assertEqual "nested handle" "{\"x\":1,\"y\":\"foo\"}" result
    , testCase "should accept nested window handle" $
        withChromiumPage $ \page -> do
          foo <- evaluateHandle page "() => window"
          let arg = argumentWithHandles [foo] (\nodes -> objectValue [("foo", nodes !! 0)])
          assertEqual "nested window" True =<< evaluateBoolWith page "({ foo }) => foo === window" arg
    , testCase "should accept multiple nested handles" $
        withChromiumPage $ \page -> do
          foo <- evaluateHandle page "() => ({ x: 1, y: 'foo' })"
          bar <- evaluateHandle page "() => 5"
          baz <- evaluateHandle page "() => (['baz'])"
          let arg =
                argumentWithHandles [foo, bar, baz] $ \nodes ->
                  objectValue
                    [ ("a1", objectValue [("foo", nodes !! 0)])
                    , ("a2", objectValue [("bar", nodes !! 1), ("arr", arrayValue [objectValue [("baz", nodes !! 2)]])])
                    ]
          result <- evaluateTextWith page "x => JSON.stringify(x)" arg
          assertEqual
            "multiple nested handles"
            "{\"a1\":{\"foo\":{\"x\":1,\"y\":\"foo\"}},\"a2\":{\"bar\":5,\"arr\":[{\"baz\":[\"baz\"]}]}}"
            result
    , testCase "should accept same handle multiple times" $
        withChromiumPage $ \page -> do
          foo <- evaluateHandle page "() => 1"
          let arg =
                argumentWithHandles [foo, foo, foo] $ \nodes ->
                  objectValue
                    [ ("foo", nodes !! 0)
                    , ("bar", arrayValue [nodes !! 1])
                    , ("baz", objectValue [("foo", nodes !! 2)])
                    ]
          result <- evaluateTextWith page "x => JSON.stringify(x)" arg
          assertEqual "same handle" "{\"foo\":1,\"bar\":[1],\"baz\":{\"foo\":1}}" result
    , testCase "should accept same nested object multiple times" $
        withChromiumPage $ \page -> do
          -- Upstream sends one shared object by reference. The Haskell argument tree repeats it
          -- in full, which the page sees as the same value.
          let foo = objectValue [("x", numberValue 1)]
              arg =
                objectArgument
                  [ ("foo", foo)
                  , ("bar", arrayValue [foo])
                  , ("baz", objectValue [("foo", foo)])
                  ]
          result <- evaluateTextWith page "x => JSON.stringify(x)" arg
          assertEqual "same nested object" "{\"foo\":{\"x\":1},\"bar\":[{\"x\":1}],\"baz\":{\"foo\":{\"x\":1}}}" result
    , testCase "should accept object handle to unserializable value" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => Infinity"
          assertEqual "infinity" True =<< evaluateBoolWith page "e => Object.is(e, Infinity)" (jsHandleArgument handle)
    , testCase "should pass configurable args" $
        withChromiumPage $ \page -> do
          let arg = objectNumberArgument [("foo", 42)]
          assertEqual "configurable" "{}"
            =<< evaluateTextWith
              page
              "arg => {\n\
              \  if (arg.foo !== 42) throw new Error('Not a 42');\n\
              \  arg.foo = 17;\n\
              \  if (arg.foo !== 17) throw new Error('Not 17');\n\
              \  delete arg.foo;\n\
              \  if (arg.foo === 17) throw new Error('Still 17');\n\
              \  return JSON.stringify(arg);\n\
              \}"
              arg
    , testCase "should work with primitives" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => { window['FOO'] = 123; return window; }"
          assertEqual "primitive" (123 :: Double) =<< evaluateNumberWith page "e => e['FOO']" (jsHandleArgument handle)
    ]

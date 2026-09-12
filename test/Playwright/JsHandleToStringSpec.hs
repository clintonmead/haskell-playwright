module Playwright.JsHandleToStringSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( PropertyName (PropertyName)
  , armConsole
  , consoleMessageText
  , evaluateHandle
  , evaluateHandleExpression
  , getProperty
  , jsHandleToString
  , waitUntilJsHandlePreview
  )
import Playwright.Harness
  ( withChromiumPage
  , assertBool
  , assertEqual
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
    "jshandle-to-string"
    [ testCase "should work for primitives" $
        withChromiumPage $ \page -> do
          numberHandle <- evaluateHandle page "() => 2"
          assertEqual "number" "2" =<< jsHandleToString numberHandle
          stringHandle <- evaluateHandle page "() => 'a'"
          assertEqual "string" "a" =<< jsHandleToString stringHandle
    , testCase "should work for complicated objects" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => window"
          assertEqual "window" "Window" =<< jsHandleToString handle
    , testCase "should beautifully render sparse arrays" $
        withChromiumPage $ \page -> do
          takeMessage <- armConsole page
          _ <-
            evaluateHandle
              page
              "() => { const a = []; a[1] = 1; a[10] = 2; a[100] = 3; console.log(a); }"
          message <- takeMessage
          assertEqual
            "sparse"
            "[empty, 1, empty x 8, 2, empty x 89, 3]"
            (consoleMessageText message)
    , testCase "should work for promises" $
        withChromiumPage $ \page -> do
          wrapper <- evaluateHandle page "() => ({ b: Promise.resolve(123) })"
          bHandle <- getProperty wrapper (PropertyName "b")
          assertEqual "promise" "Promise" =<< jsHandleToString bHandle
    , testCase "should work with different subtypes" $
        withChromiumPage $ \page -> do
          functionPreview <- jsHandleToString =<< evaluateHandleExpression page "(function(){})"
          assertBool "function" ("function" `isInfixOf` functionPreview)
          assertEqual "12" "12" =<< jsHandleToString =<< evaluateHandleExpression page "12"
          assertEqual "true" "true" =<< jsHandleToString =<< evaluateHandleExpression page "true"
          assertEqual "undefined" "undefined" =<< jsHandleToString =<< evaluateHandleExpression page "undefined"
          assertEqual "foo" "foo" =<< jsHandleToString =<< evaluateHandleExpression page "\"foo\""
          assertEqual "symbol" "Symbol()" =<< jsHandleToString =<< evaluateHandleExpression page "Symbol()"
          mapPreview <- jsHandleToString =<< evaluateHandleExpression page "new Map()"
          assertBool "Map" ("Map" `isInfixOf` mapPreview)
          setPreview <- jsHandleToString =<< evaluateHandleExpression page "new Set()"
          assertBool "Set" ("Set" `isInfixOf` setPreview)
          arrayPreview <- jsHandleToString =<< evaluateHandleExpression page "[]"
          assertBool "Array" ("Array" `isInfixOf` arrayPreview)
          assertEqual "null" "null" =<< jsHandleToString =<< evaluateHandleExpression page "null"
          bodyHandle <- evaluateHandleExpression page "document.body"
          resolved <- waitUntilJsHandlePreview bodyHandle (== "JSHandle@<body></body>")
          assertEqual "body" "JSHandle@<body></body>" resolved
          assertEqual "WeakMap" "WeakMap" =<< jsHandleToString =<< evaluateHandleExpression page "new WeakMap()"
          assertEqual "WeakSet" "WeakSet" =<< jsHandleToString =<< evaluateHandleExpression page "new WeakSet()"
          errorPreview <- jsHandleToString =<< evaluateHandleExpression page "new Error()"
          assertBool "Error" ("Error" `isInfixOf` errorPreview)
          assertEqual "Proxy" "Proxy(Object)" =<< jsHandleToString =<< evaluateHandleExpression page "new Proxy({}, {})"
    , testCase "should work with previewable subtypes" $
        withChromiumPage $ \page -> do
          assertEqual "regex" "/foo/" =<< jsHandleToString =<< evaluateHandleExpression page "/foo/"
          datePreview <- jsHandleToString =<< evaluateHandleExpression page "new Date(0)"
          assertBool "GMT" ("GMT" `isInfixOf` datePreview)
          int32Preview <- jsHandleToString =<< evaluateHandleExpression page "new Int32Array()"
          assertBool "Int32Array" ("Int32Array" `isInfixOf` int32Preview)
    ]

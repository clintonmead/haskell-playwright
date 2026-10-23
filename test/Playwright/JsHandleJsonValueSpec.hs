module Playwright.JsHandleJsonValueSpec
  ( tests
  ) where

import Data.Text
  ( Text
  )
import Playwright
  ( evaluateHandle
  , jsonValue
  )
import Playwright.Harness
  ( withChromiumPage
  , assertBool
  , assertEqual
  , assertFailure
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadDate, PayloadObject, PayloadString)
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
    "jshandle-json-value"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => ({ foo: 'bar' })"
          value <- jsonValue handle
          case value of
            SerializedNode (PayloadObject fields) _ ->
              assertEqual "foo" (Just ("bar" :: Text)) (lookupText "foo" fields)
            _ ->
              assertFailure "expected object"
    , testCase "should work with dates" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => new Date('2017-09-26T00:00:00.000Z')"
          value <- jsonValue handle
          case value of
            SerializedNode (PayloadDate stamp) _ ->
              assertEqual "date" ("2017-09-26T00:00:00.000Z" :: Text) stamp
            _ ->
              assertFailure "expected date"
    , testCase "should handle circular objects" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => { const a = {}; a.b = a; return a; }"
          value <- jsonValue handle
          assertBool "circular" (isSelfCycle value)
    ]

isSelfCycle :: SerializedValue -> Bool
isSelfCycle value =
  case value of
    SerializedNode (PayloadObject [("b", SerializedRef target)]) (Just self) ->
      target == self
    _ ->
      False

lookupText :: Text -> [(Text, SerializedValue)] -> Maybe Text
lookupText key fields =
  case lookup key fields of
    Just (SerializedNode (PayloadString text) _) ->
      Just text
    _ ->
      Nothing

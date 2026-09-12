module Playwright.JsHandlePropertiesSpec
  ( tests
  ) where

import Playwright
  ( PropertyName (PropertyName)
  , Playwright
  , asElement
  , evaluateHandle
  , getProperty
  , getPropertyList
  , jsonValueNumber
  , jsonValueSpecial
  , jsonValueText
  , setContent
  , textContentHandle
  )
import Playwright.Harness
  ( requireHandle
  , withChromiumPage
  , assertEqual
  , assertFailure
  )
import Playwright.SpecialJsValue
  ( SpecialJsValue (SpecialInfinity, SpecialNaN, SpecialNegInfinity, SpecialNegZero, SpecialNull, SpecialUndefined)
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
    "jshandle-properties"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => ({ one: 1, two: 2, three: 3 })"
          two <- getProperty handle (PropertyName "two")
          assertEqual "two" (2 :: Double) =<< jsonValueNumber two
    , testCase "should work with undefined, null, and empty" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => ({ undefined: undefined, null: null })"
          undefinedHandle <- getProperty handle (PropertyName "undefined")
          assertEqual "undefined" SpecialUndefined =<< jsonValueSpecial undefinedHandle
          nullHandle <- getProperty handle (PropertyName "null")
          assertEqual "null" SpecialNull =<< jsonValueSpecial nullHandle
          emptyHandle <- getProperty handle (PropertyName "empty")
          assertEqual "empty" SpecialUndefined =<< jsonValueSpecial emptyHandle
    , testCase "should work with unserializable values" $
        withChromiumPage $ \page -> do
          handle <-
            evaluateHandle
              page
              "() => ({ infinity: Infinity, nInfinity: -Infinity, nan: NaN, nzero: -0 })"
          assertEqual "Infinity" SpecialInfinity
            =<< jsonValueSpecial =<< getProperty handle (PropertyName "infinity")
          assertEqual "-Infinity" SpecialNegInfinity
            =<< jsonValueSpecial =<< getProperty handle (PropertyName "nInfinity")
          assertEqual "NaN" SpecialNaN
            =<< jsonValueSpecial =<< getProperty handle (PropertyName "nan")
          assertEqual "-0" SpecialNegZero
            =<< jsonValueSpecial =<< getProperty handle (PropertyName "nzero")
    , testCase "getProperties should work" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => ({ foo: 'bar' })"
          foo <- requireProperty (PropertyName "foo") =<< getPropertyList handle
          assertEqual "foo" "bar" =<< jsonValueText foo
    , testCase "getProperties should return empty map for non-objects" $
        withChromiumPage $ \page -> do
          handle <- evaluateHandle page "() => 123"
          props <- getPropertyList handle
          assertEqual "empty" (0 :: Int) (length props)
    , testCase "getProperties should return even non-own properties" $
        withChromiumPage $ \page -> do
          handle <-
            evaluateHandle
              page
              "() => { class A { constructor() { this.a = '1'; } } class B extends A { constructor() { super(); this.b = '2'; } } return new B(); }"
          props <- getPropertyList handle
          a <- requireProperty (PropertyName "a") props
          b <- requireProperty (PropertyName "b") props
          assertEqual "a" "1" =<< jsonValueText a
          assertEqual "b" "2" =<< jsonValueText b
    , testCase "getProperties should work with elements" $
        withChromiumPage $ \page -> do
          setContent page "<div>Hello</div>"
          handle <- evaluateHandle page "() => ({ body: document.body })"
          bodyHandle <- requireProperty (PropertyName "body") =<< getPropertyList handle
          body <- requireHandle =<< asElement bodyHandle
          assertEqual "hello" (Just "Hello") =<< textContentHandle body
    ]

requireProperty :: PropertyName -> [(PropertyName, a)] -> Playwright a
requireProperty name props =
  case lookup name props of
    Nothing ->
      assertFailure "missing property"
    Just value ->
      pure value

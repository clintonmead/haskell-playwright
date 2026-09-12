module Playwright.PageExposeFunctionSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( throwIO
  )
import Playwright
  ( BindingName (BindingName)
  , defaultGotoOptions
  , dispose
  , evaluateNumber
  , evaluateOnFrame
  , exposeFunction
  , frames
  , goto
  , numberArgument
  , setContent
  , urlFromText
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssets
  , withChromiumPage
  , assertEqual
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber)
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
    "page-expose-function"
    [ testCase "should work" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "compute") multiply
          assertEqual "product" (36 :: Double)
            =<< evaluateNumber page "async () => window.compute(9, 4)"
    , testCase "should dispose" $
        withChromiumPage $ \page -> do
          binding <- exposeFunction page (BindingName "compute") multiply
          assertEqual "product" (36 :: Double)
            =<< evaluateNumber page "async () => window.compute(9, 4)"
          dispose binding
          assertProtocolContains "is not a function" (evaluateNumber page "async () => window.compute(9, 4)")
    , testCase "should survive navigation" $
        withAssets $ \prefix page -> do
          _ <- exposeFunction page (BindingName "compute") multiply
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "product" (36 :: Double)
            =<< evaluateNumber page "async () => window.compute(9, 4)"
    , testCase "should work on frames" $
        withAssets $ \prefix page -> do
          _ <- exposeFunction page (BindingName "compute") multiply
          _ <- goto page (urlFromText (prefix <> "/frames/nested-frames.html")) defaultGotoOptions
          children <- frames page
          case children of
            (_ : frame : _) -> do
              value <- evaluateOnFrame frame "async () => window.compute(3, 5)"
              assertEqual "product" (15 :: Double) =<< requireNumber value
            _ ->
              assertEqual "frames" True False
    , testCase "should throw for duplicate registrations" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "foo") multiply
          assertProtocolContains
            "has been already registered"
            (exposeFunction page (BindingName "foo") multiply)
    , testCase "should work with setContent" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "compute") multiply
          setContent page "<script>window.result = compute(3, 2)</script>"
          assertEqual "product" (6 :: Double) =<< evaluateNumber page "() => window.result"
    ]

multiply :: [SerializedValue] -> Playwright SerializedArgument
multiply args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left * right))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

requireNumber :: SerializedValue -> Playwright Double
requireNumber value =
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)

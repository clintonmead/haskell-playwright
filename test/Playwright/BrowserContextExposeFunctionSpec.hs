module Playwright.BrowserContextExposeFunctionSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( throwIO
  )
import Playwright
  ( BindingName (BindingName)
  , contextExposeFunction
  , defaultGotoOptions
  , defaultNewContextOptions
  , evaluateNumber
  , exposeFunction
  , goto
  , numberArgument
  , urlFromText
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Harness
  ( assertProtocolContains
  , withAssetsContextWith
  , withChromiumContext
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
    "browsercontext-expose-function"
    [ testCase "should work" $
        withChromiumContext $ \ctx page -> do
          _ <- contextExposeFunction ctx (BindingName "add") add
          assertEqual "sum" (11 :: Double)
            =<< evaluateNumber page "async () => window.add(5, 6)"
    , testCase "should throw for duplicate registrations" $
        withChromiumContext $ \ctx _ -> do
          _ <- contextExposeFunction ctx (BindingName "foo") add
          assertProtocolContains
            "has been already registered"
            (contextExposeFunction ctx (BindingName "foo") add)
    , testCase "should work alongside page.exposeFunction" $
        withChromiumContext $ \ctx page -> do
          _ <- contextExposeFunction ctx (BindingName "add") add
          _ <- exposeFunction page (BindingName "mul") multiply
          assertEqual "sum" (11 :: Double)
            =<< evaluateNumber page "async () => window.add(5, 6)"
          assertEqual "product" (20 :: Double)
            =<< evaluateNumber page "async () => window.mul(4, 5)"
    , testCase "should survive navigation" $
        withAssetsContextWith defaultNewContextOptions $ \prefix ctx page -> do
          _ <- contextExposeFunction ctx (BindingName "add") add
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "sum" (11 :: Double)
            =<< evaluateNumber page "async () => window.add(5, 6)"
    ]

add :: [SerializedValue] -> Playwright SerializedArgument
add args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left + right))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

multiply :: [SerializedValue] -> Playwright SerializedArgument
multiply args =
  case args of
    [SerializedNode (PayloadNumber left) _, SerializedNode (PayloadNumber right) _] ->
      pure (numberArgument (left * right))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

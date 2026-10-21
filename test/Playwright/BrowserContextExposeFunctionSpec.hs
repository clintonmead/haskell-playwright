module Playwright.BrowserContextExposeFunctionSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Network.HTTP.Types
  ( hContentType
  , status200
  )
import Network.Wai
  ( Application
  , responseLBS
  )
import UnliftIO.IORef
  ( modifyIORef'
  , newIORef
  , readIORef
  )
import UnliftIO.Exception
  ( throwIO
  , try
  )
import Playwright
  ( BindingName (BindingName)
  , newPage
  , undefinedArgument
  , evaluateText
  , dispose
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
  ( PlaywrightError (EvaluateUnexpected, ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertBool
  , withLocalApp
  , assertFailure
  , assertProtocolContains
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
    , testCase "should dispose" $
        withChromiumContext $ \ctx page -> do
          binding <- contextExposeFunction ctx (BindingName "compute") multiply
          assertEqual "before" (36 :: Double) =<< evaluateNumber page "async () => window.compute(9, 4)"
          dispose binding
          result <- try (evaluateNumber page "async () => window.compute(9, 4)") :: Playwright (Either PlaywrightError Double)
          case result of
            Left (ProtocolRpc failure) ->
              assertBool "removed" ("is not a function" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected the disposed function to be gone"
    , testCase "should work with CSP" $
        withChromiumContext $ \ctx _ ->
          withLocalApp cspEmptyApp $ \base -> do
            page <- newPage ctx
            _ <- goto page (urlFromText (base <> "/empty.html")) defaultGotoOptions
            called <- newIORef False
            _ <- contextExposeFunction ctx (BindingName "hi") $ \_ -> do
              modifyIORef' called (const True)
              pure undefinedArgument
            assertEqual "returned" "done" =<< evaluateText page "async () => { await window.hi(); return 'done'; }"
            assertEqual "called" True =<< readIORef called
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

-- | Serves an empty page with a Content-Security-Policy header.
cspEmptyApp :: Application
cspEmptyApp _ respond =
  respond
    ( responseLBS
        status200
        [ ("Content-Security-Policy", "default-src \"self\"")
        , (hContentType, "text/html")
        ]
        ""
    )

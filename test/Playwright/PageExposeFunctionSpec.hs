module Playwright.PageExposeFunctionSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( throwIO
  )
import Data.Text
  ( isInfixOf
  , replace
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , takeMVar
  , tryPutMVar
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.Maybe
  ( isJust
  )
import System.Timeout
  ( timeout
  )
import Playwright
  ( BindingName (BindingName)
  , SpecialJsValue (SpecialUndefined)
  , addInitScript
  , evaluateText
  , reload
  , specialArgument
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
  , assertBool
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
    , testCase "should throw exception in page context" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "woof") (\_ -> throwIO (userError "WOOF WOOF"))
          message <-
            evaluateText
              page
              "async () => { try { await window.woof(); return 'no error'; } catch (e) { return e.message; } }"
          assertBool "message" ("WOOF WOOF" `isInfixOf` message)
    , testCase "should be callable from-inside addInitScript" $
        withChromiumPage $ \page -> do
          called <- newEmptyMVar
          _ <-
            exposeFunction
              page
              (BindingName "woof")
              (\_ -> tryPutMVar called () >> pure (specialArgument SpecialUndefined))
          addInitScript page "(() => { window.woof(); })();"
          _ <- reload page
          reached <- liftIO (timeout 10000000 (takeMVar called))
          assertBool "called from the init script" (isJust reached)
    , testCase "should work after cross origin navigation" $
        withAssets $ \prefix page -> do
          _ <- exposeFunction page (BindingName "compute") multiply
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          -- localhost is a different origin from 127.0.0.1, and the same server answers both.
          _ <- goto page (urlFromText (replace "127.0.0.1" "localhost" prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "product" (36 :: Double)
            =<< evaluateNumber page "async () => window.compute(9, 4)"
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

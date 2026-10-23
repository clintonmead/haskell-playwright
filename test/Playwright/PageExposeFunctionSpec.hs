module Playwright.PageExposeFunctionSpec
  ( tests
  ) where

import UnliftIO.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  , isInfixOf
  , replace
  )
import UnliftIO.Async
  ( concurrently_
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
  , evaluate
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
  , objectNumberArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadArray, PayloadNumber, PayloadObject, PayloadString)
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
    , testCase "should work on frames before navigation" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/nested-frames.html")) defaultGotoOptions
          _ <- exposeFunction page (BindingName "compute") multiply
          children <- frames page
          case children of
            (_ : frame : _) -> do
              value <- evaluateOnFrame frame "async () => window.compute(3, 5)"
              assertEqual "product" (15 :: Double) =<< requireNumber value
            _ ->
              assertEqual "frames" True False
    , testCase "should await returned promise" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "compute") multiply
          assertEqual "product" (15 :: Double) =<< evaluateNumber page "async () => await window.compute(3, 5)"
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
    , testCase "should work with overridden console object" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { window.console = null; }"
          assertEqual "console" "true" =<< evaluateText page "() => String(window.console === null)"
          _ <- exposeFunction page (BindingName "compute") multiply
          assertEqual "product" (30 :: Double)
            =<< evaluateNumber page "async () => window.compute(5, 6)"
    , testCase "should work with busted Array.prototype.map/push" $
        withChromiumPage $ \page -> do
          setContent page "<script>Array.prototype.map = null; Array.prototype.push = null;</script>"
          _ <- exposeFunction page (BindingName "compute") multiply
          assertEqual "product" (30 :: Double)
            =<< evaluateNumber page "async () => window.compute(5, 6)"
    , testCase "exposeBinding should work in parallel" $
        withChromiumPage $ \page -> do
          concurrently_
            (exposeFunction page (BindingName "foo") constant42)
            (exposeFunction page (BindingName "bar") constant42)
          assertEqual "sum" (84 :: Double)
            =<< evaluateNumber page "async () => (await window.foo()) + (await window.bar())"
    , testCase "should alias Window, Document and Node" $
        withChromiumPage $ \page -> do
          captured <- newEmptyMVar
          _ <-
            exposeFunction
              page
              (BindingName "log")
              (\args -> tryPutMVar captured args >> pure (specialArgument SpecialUndefined))
          _ <- evaluate page "() => window.log([window, document, document.body])"
          args <- liftIO (timeout 10000000 (takeMVar captured))
          assertEqual "aliases" (Just ["ref: <Window>", "ref: <Document>", "ref: <Node>"]) (aliasNames <$> args)
    , testCase "should serialize cycles" $
        withChromiumPage $ \page -> do
          captured <- newEmptyMVar
          _ <-
            exposeFunction
              page
              (BindingName "log")
              (\args -> tryPutMVar captured args >> pure (specialArgument SpecialUndefined))
          _ <- evaluate page "() => { const a = {}; a.b = a; window.log(a); }"
          args <- liftIO (timeout 10000000 (takeMVar captured))
          assertBool "cycle" (maybe False isCycle args)
    , testCase "should work with complex objects" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "complexObject") sumX
          assertEqual "x" (7 :: Double)
            =<< evaluateNumber page "async () => (await window.complexObject({ x: 5 }, { x: 2 })).x"
    , testCase "should fail with busted Array.prototype.toJSON" $
        withChromiumPage $ \page -> do
          _ <- evaluate page "() => { Array.prototype.toJSON = () => '\"[]\"'; }"
          _ <- exposeFunction page (BindingName "add") multiply
          assertProtocolContains
            "serializedArgs is not an array"
            (evaluateNumber page "async () => window.add(5, 6)")
          assertEqual "toJSON" "\"[]\"" =<< evaluateText page "() => [].toJSON()"
    ]

-- | The names of the three aliased objects, from the one array argument that
-- @window.log([window, document, document.body])@ sends.
aliasNames :: [SerializedValue] -> [Text]
aliasNames args =
  case args of
    [SerializedNode (PayloadArray items) _] ->
      [text | SerializedNode (PayloadString text) _ <- items]
    _ ->
      []

-- | True when the one argument is an object whose @b@ field refers back to the
-- object itself, which is how the driver encodes @const a = {}; a.b = a@.
isCycle :: [SerializedValue] -> Bool
isCycle args =
  case args of
    [SerializedNode (PayloadObject [("b", SerializedRef target)]) (Just self)] ->
      target == self
    _ ->
      False

-- | Adds the @x@ fields of two object arguments and returns an object with the sum.
sumX :: [SerializedValue] -> Playwright SerializedArgument
sumX args =
  case args of
    [SerializedNode (PayloadObject [("x", SerializedNode (PayloadNumber left) _)]) _, SerializedNode (PayloadObject [("x", SerializedNode (PayloadNumber right) _)]) _] ->
      pure (objectNumberArgument [("x", left + right)])
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

constant42 :: [SerializedValue] -> Playwright SerializedArgument
constant42 _ = pure (numberArgument 42)

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

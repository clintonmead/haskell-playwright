-- | Ports tests/page/page-evaluate-callback.spec.ts. Upstream passes functions
-- as evaluate arguments with `exposeFunctions`. Haskell has no function
-- serialization, so each callback is exposed with `exposeFunction` and the
-- page calls it as `window.<name>`. Skipped upstream: the throw without the
-- option (no such option in Haskell), the whole-argument function (functions
-- cannot be passed as evaluate arguments), and the check on the internal
-- `__pw_fn_` global name, which Haskell does not have.
module Playwright.PageEvaluateCallbackSpec
  ( tests
  ) where

import Control.Concurrent
  ( threadDelay
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import Data.Text
  ( Text
  , isInfixOf
  )
import Playwright
  ( BindingName (BindingName)
  , SpecialJsValue (SpecialUndefined)
  , childLocator
  , defaultGotoOptions
  , evaluate
  , evaluateHandle
  , evaluateHandleOnLocator
  , evaluateHandleWith
  , evaluateNumber
  , evaluateOnFrame
  , evaluateOnHandle
  , evaluateOnHandleText
  , evaluateOnLocator
  , evaluateText
  , exposeFunction
  , frameLocator
  , goto
  , jsHandleArgument
  , jsHandleObjectArgument
  , locator
  , numberArgument
  , selectorFromText
  , setContent
  , specialArgument
  , textArgument
  , urlFromText
  , Page
  , Playwright
  )
import Playwright.Error
  ( PlaywrightError (EvaluateUnexpected)
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , attachFrame
  , withAssets
  , withChromiumPage
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.SerializedValue
  ( SerializedPayload (PayloadNumber, PayloadString)
  , SerializedValue (SerializedNode)
  )
import System.Timeout
  ( timeout
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( throwIO
  )
import UnliftIO.MVar
  ( MVar
  , newEmptyMVar
  , takeMVar
  , tryPutMVar
  )

tests :: TestTree
tests =
  testGroup
    "page-evaluate-callback"
    [ testCase "should call a function passed as an argument" $
        withChromiumPage $ \page -> do
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordNumber received)
          _ <- evaluate page "async () => { await window.cb(1); await window.cb(2); }"
          assertEqual "received" [1, 2] =<< liftIO (readIORef received)
    , testCase "should pass arguments to the callback" $
        withChromiumPage $ \page -> do
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordValues received)
          _ <- evaluate page "async () => { await window.cb(1, 'two', { three: 3 }, [4]); }"
          values <- liftIO (readIORef received)
          assertEqual "argument count" (4 :: Int) (length values)
          case values of
            first : _ -> assertEqual "first argument" (1 :: Double) =<< requireNumber first
            [] -> assertBool "arguments" False
    , testCase "should return the callback result to the page" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "cb") doubleIt
          assertEqual "result" (42 :: Double) =<< evaluateNumber page "async () => await window.cb(21)"
    , testCase "should support multiple callbacks" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "add") (binaryNumber (+))
          _ <- exposeFunction page (BindingName "mul") (binaryNumber (*))
          assertEqual "result" (11 :: Double) =<< evaluateNumber page
            "async () => (await window.add(2, 3)) + (await window.mul(2, 3))"
    , testCase "should support handle as a callback result" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "cb") (doubleHandle page)
          assertEqual "result" (101 :: Double) =<< evaluateNumber page "async () => (await window.cb(42)) + 17"
    , testCase "should support nested handles in the callback result" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "cb") (nestedDoubleHandle page)
          assertEqual "result" (101 :: Double) =<< evaluateNumber page
            "async () => { const res = await window.cb(42); return res.double + 17; }"
    , testCase "should await an async callback result" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "cb") addOneSlowly
          assertEqual "value" (21 :: Double) =<< evaluateNumber page "async () => await window.cb(20)"
    , testCase "should propagate callback errors to the page" $
        withChromiumPage $ \page -> do
          _ <- exposeFunction page (BindingName "cb") (\_ -> throwIO (userError "boom"))
          message <-
            evaluateText
              page
              "async () => { try { await window.cb(); return 'no error'; } catch (e) { return e.message; } }"
          assertBool "message" ("boom" `isInfixOf` message)
    , testCase "should work with a fire-and-forget setTimeout callback" $
        withChromiumPage $ \page -> do
          value <- newEmptyMVar
          _ <- exposeFunction page (BindingName "cb") (signalNumber value)
          _ <- evaluate page "() => { setTimeout(() => window.cb(5), 0); }"
          reached <- liftIO (timeout 10000000 (takeMVar value))
          assertEqual "value" (Just (5 :: Double)) reached
    , testCase "should work with evaluateHandle" $
        withChromiumPage $ \page -> do
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordNumber received)
          handle <- evaluateHandle page "async () => { await window.cb(7); return { done: true }; }"
          assertEqual "json" "{\"done\":true}" =<< evaluateOnHandleText handle "value => JSON.stringify(value)"
          assertEqual "received" [7] =<< liftIO (readIORef received)
    , testCase "should work in a child frame" $
        withAssets $ \prefix page -> do
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordNumber received)
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page "frame1" (prefix <> "/empty.html")
          _ <- evaluateOnFrame frame "async () => { await window.cb(42); }"
          assertEqual "received" [42] =<< liftIO (readIORef received)
    , testCase "should route callbacks back to the calling frame" $
        withAssets $ \prefix page -> do
          _ <- exposeFunction page (BindingName "greet") greet
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page "frame1" (prefix <> "/empty.html")
          fromMain <- evaluateText page "async () => await window.greet('main')"
          fromChild <- requireText =<< evaluateOnFrame frame "async () => await window.greet('child')"
          assertEqual "main" "hello main" fromMain
          assertEqual "child" "hello child" fromChild
    , testCase "should work with jsHandle.evaluate" $
        withChromiumPage $ \page -> do
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordNumber received)
          handle <- evaluateHandle page "() => window"
          _ <- evaluateOnHandle handle "async win => { await window.cb(99); }"
          assertEqual "received" [99] =<< liftIO (readIORef received)
    , testCase "should work with locator.evaluate" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target>hello</div>"
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordText received)
          _ <-
            evaluateOnLocator
              (locator page (selectorFromText "#target"))
              "async element => { await window.cb(element.id); }"
          assertEqual "received" ["target"] =<< liftIO (readIORef received)
    , testCase "should return the callback result with locator.evaluate" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target>7</div>"
          _ <- exposeFunction page (BindingName "double") doubleIt
          result <-
            requireNumber
              =<< evaluateOnLocator
                (locator page (selectorFromText "#target"))
                "async element => await window.double(+element.textContent)"
          assertEqual "result" (14 :: Double) result
    , testCase "should propagate callback errors with locator.evaluate" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target></div>"
          _ <- exposeFunction page (BindingName "cb") (\_ -> throwIO (userError "boom"))
          message <-
            requireText
              =<< evaluateOnLocator
                (locator page (selectorFromText "#target"))
                "async () => { try { await window.cb(); return 'no error'; } catch (e) { return e.message; } }"
          assertBool "message" ("boom" `isInfixOf` message)
    , testCase "should work with locator.evaluateHandle" $
        withChromiumPage $ \page -> do
          setContent page "<div id=target>hello</div>"
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordText received)
          handle <-
            evaluateHandleOnLocator
              (locator page (selectorFromText "#target"))
              "async element => { await window.cb(element.id); return element; }"
          assertEqual "received" ["target"] =<< liftIO (readIORef received)
          assertEqual "id" "target" =<< evaluateOnHandleText handle "element => element.id"
    , testCase "should work with locator.evaluate inside an iframe" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page "frame1" (prefix <> "/empty.html")
          _ <- evaluateOnFrame frame "() => { document.body.innerHTML = '<div id=target>in-frame</div>'; }"
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordText received)
          _ <-
            evaluateOnLocator
              (childLocator (frameLocator page (selectorFromText "#frame1")) (selectorFromText "#target"))
              "async element => { await window.cb(element.textContent); }"
          assertEqual "received" ["in-frame"] =<< liftIO (readIORef received)
    , testCase "should survive a navigation and keep working" $
        withAssets $ \prefix page -> do
          received <- liftIO (newIORef [])
          _ <- exposeFunction page (BindingName "cb") (recordNumber received)
          _ <- evaluate page "async () => { await window.cb(1); }"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluate page "async () => { await window.cb(2); }"
          assertEqual "received" [1, 2] =<< liftIO (readIORef received)
    , testCase "should scope the page-side callback to the execution context" $
        withAssets $ \prefix page -> do
          _ <- exposeFunction page (BindingName "cb") (\_ -> pure (specialArgument SpecialUndefined))
          _ <- evaluate page "() => { window.__cb = window.cb; }"
          assertEqual "before navigation" "function" =<< evaluateText page "() => typeof window.__cb"
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          assertEqual "after navigation" "undefined" =<< evaluateText page "() => typeof window.__cb"
    ]

-- | Records the one number the page passes.
recordNumber :: IORef [Double] -> [SerializedValue] -> Playwright SerializedArgument
recordNumber received args = do
  number <- singleNumber args
  liftIO (modifyIORef' received (<> [number]))
  pure (specialArgument SpecialUndefined)

-- | Records the one text the page passes.
recordText :: IORef [Text] -> [SerializedValue] -> Playwright SerializedArgument
recordText received args = do
  text <- singleText args
  liftIO (modifyIORef' received (<> [text]))
  pure (specialArgument SpecialUndefined)

-- | Keeps every argument the page passes, so the test can count them.
recordValues :: IORef [SerializedValue] -> [SerializedValue] -> Playwright SerializedArgument
recordValues received args = do
  liftIO (writeIORef received args)
  pure (specialArgument SpecialUndefined)

-- | Answers with the given number, after the page's first argument.
doubleIt :: [SerializedValue] -> Playwright SerializedArgument
doubleIt args = do
  number <- singleNumber args
  pure (numberArgument (number * 2))

-- | Answers with a handle to twice the page's number, created in the page.
doubleHandle :: Page -> [SerializedValue] -> Playwright SerializedArgument
doubleHandle page args = do
  number <- singleNumber args
  handle <- evaluateHandleWith page "x => 2 * x" (numberArgument number)
  pure (jsHandleArgument handle)

-- | Answers with an object whose field is a handle to twice the page's number.
nestedDoubleHandle :: Page -> [SerializedValue] -> Playwright SerializedArgument
nestedDoubleHandle page args = do
  number <- singleNumber args
  handle <- evaluateHandleWith page "x => 2 * x" (numberArgument number)
  pure (jsHandleObjectArgument "double" handle)

-- | Answers with the first number plus one, after a short pause.
addOneSlowly :: [SerializedValue] -> Playwright SerializedArgument
addOneSlowly args = do
  number <- singleNumber args
  liftIO (threadDelay 10000)
  pure (numberArgument (number + 1))

-- | Answers with the result of the operation on the two numbers.
binaryNumber :: (Double -> Double -> Double) -> [SerializedValue] -> Playwright SerializedArgument
binaryNumber op args =
  case args of
    [left, right] -> do
      a <- requireNumber left
      b <- requireNumber right
      pure (numberArgument (op a b))
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

-- | Stores the number and reports that the call happened.
signalNumber :: MVar Double -> [SerializedValue] -> Playwright SerializedArgument
signalNumber value args = do
  number <- singleNumber args
  _ <- tryPutMVar value number
  pure (specialArgument SpecialUndefined)

greet :: [SerializedValue] -> Playwright SerializedArgument
greet args = do
  name <- singleText args
  pure (textArgument ("hello " <> name))

singleNumber :: [SerializedValue] -> Playwright Double
singleNumber args =
  case args of
    [value] -> requireNumber value
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

singleText :: [SerializedValue] -> Playwright Text
singleText args =
  case args of
    [value] -> requireText value
    _ ->
      throwIO (EvaluateUnexpected (SerializedNode (PayloadNumber 0) Nothing))

requireNumber :: SerializedValue -> Playwright Double
requireNumber value =
  case value of
    SerializedNode (PayloadNumber number) _ ->
      pure number
    _ ->
      throwIO (EvaluateUnexpected value)

requireText :: SerializedValue -> Playwright Text
requireText value =
  case value of
    SerializedNode (PayloadString text) _ ->
      pure text
    _ ->
      throwIO (EvaluateUnexpected value)

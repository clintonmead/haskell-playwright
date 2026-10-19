module Playwright.FrameEvaluateSpec
  ( tests
  ) where

import Playwright
  ( defaultGotoOptions
  , evaluateHandle
  , evaluateOnFrameText
  , frames
  , goto
  , setContent
  , urlFromText
  )
import Playwright.Harness
  ( attachFrame
  , withAssets
  , assertEqual
  , assertFailure
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
    "frame-evaluate"
    [ testCase "should have different execution contexts" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page ("<iframe src=\"" <> prefix <> "/empty.html\"></iframe>")
          allFrames <- frames page
          case allFrames of
            [parentFrame, childFrame] -> do
              _ <- evaluateOnFrameText parentFrame "() => { window.FOO = 'foo'; return 'foo'; }"
              _ <- evaluateOnFrameText childFrame "() => { window.FOO = 'bar'; return 'bar'; }"
              assertEqual "parent" "foo" =<< evaluateOnFrameText parentFrame "() => window.FOO"
              assertEqual "child" "bar" =<< evaluateOnFrameText childFrame "() => window.FOO"
            _ ->
              assertFailure ("expected 2 frames, got " <> show (length allFrames))
    , testCase "should have correct execution contexts" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          allFrames <- frames page
          case allFrames of
            [parentFrame, childFrame] -> do
              assertEqual "parent" "" =<< evaluateOnFrameText parentFrame "() => document.body.textContent.trim()"
              assertEqual "child" "Hi, I'm frame" =<< evaluateOnFrameText childFrame "() => document.body.textContent.trim()"
            _ ->
              assertFailure ("expected 2 frames, got " <> show (length allFrames))
    , testCase "should be isolated between frames" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- attachFrame page "frame1" (prefix <> "/empty.html")
          allFrames <- frames page
          case allFrames of
            [first, second] -> do
              _ <- evaluateOnFrameText first "() => { window.a = 1; return 'set'; }"
              _ <- evaluateOnFrameText second "() => { window.a = 2; return 'set'; }"
              assertEqual "first" "1" =<< evaluateOnFrameText first "() => String(window.a)"
              assertEqual "second" "2" =<< evaluateOnFrameText second "() => String(window.a)"
            _ ->
              assertFailure ("expected 2 frames, got " <> show (length allFrames))
    , testCase "evaluateHandle should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- evaluateHandle page "() => window"
          pure ()
    ]

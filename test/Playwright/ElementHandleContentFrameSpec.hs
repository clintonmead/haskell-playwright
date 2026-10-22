module Playwright.ElementHandleContentFrameSpec
  ( tests
  ) where

import Data.Text
  ( replace
  )
import Playwright
  ( asElement
  , evaluateHandleOnFrame
  , framesEqual
  , frames
  , querySelector
  , selectorFromText
  , contentFrameHandle
  , defaultGotoOptions
  , evaluateHandle
  , evaluateHandleOnFrame
  , frames
  , framesEqual
  , goto
  , querySelector
  , selectorFromText
  , setContent
  , urlFromText
  , waitForFunction
  )
import Playwright.Harness
  ( attachFrame
  , requireHandle
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
    "elementhandle-content-frame"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page ("<iframe id=frame1 src=\"" <> prefix <> "/empty.html\"></iframe>")
          _ <- waitForFunction page "() => document.querySelector('#frame1')?.contentWindow?.document?.body"
          handle <- requireHandle =<< querySelector page (selectorFromText "#frame1")
          frame <- contentFrameHandle handle
          allFrames <- frames page
          case (frame, allFrames) of
            (Just actual, _ : child : _) ->
              assertEqual "iframe" True (framesEqual actual child)
            _ ->
              assertFailure "expected iframe frame"
    , testCase "should return null for non-iframes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page ("<iframe id=frame1 src=\"" <> prefix <> "/empty.html\"></iframe>")
          jsHandle <- evaluateHandle page "() => document.body"
          element <- requireHandle =<< asElement jsHandle
          frame <- contentFrameHandle element
          assertEqual "null" True (maybe True (const False) frame)
    , testCase "should return null for document.documentElement" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          child <- attachFrame page "frame1" (prefix <> "/empty.html")
          jsHandle <- evaluateHandleOnFrame child "() => document.documentElement"
          element <- requireHandle =<< asElement jsHandle
          frame <- contentFrameHandle element
          assertEqual "null" True (maybe True (const False) frame)
    , testCase "should work for cross-frame evaluations" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- attachFrame page "frame1" (prefix <> "/empty.html")
          allFrames <- frames page
          case drop 1 allFrames of
            [] ->
              assertFailure "expected a child frame"
            child : _ -> do
              jsHandle <- evaluateHandleOnFrame child "() => window.top.document.querySelector('#frame1')"
              element <- requireHandle =<< asElement jsHandle
              owner <- contentFrameHandle element
              assertEqual "same frame" True (maybe False (`framesEqual` child) owner)
    , testCase "should work for cross-process iframes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          let cross = replace "127.0.0.1" "localhost" prefix
          _ <- attachFrame page "frame1" (cross <> "/empty.html")
          element <- requireHandle =<< querySelector page (selectorFromText "#frame1")
          content <- contentFrameHandle element
          allFrames <- frames page
          case drop 1 allFrames of
            [] ->
              assertFailure "expected a child frame"
            child : _ ->
              assertEqual "same frame" True (maybe False (`framesEqual` child) content)
    ]

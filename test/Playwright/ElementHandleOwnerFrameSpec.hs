module Playwright.ElementHandleOwnerFrameSpec
  ( tests
  ) where

import Playwright
  ( asElement
  , defaultGotoOptions
  , evaluateHandle
  , evaluateHandleOnFrame
  , frames
  , framesEqual
  , goto
  , mainFrame
  , ownerFrameHandle
  , setContent
  , urlFromText
  , waitForFunction
  )
import Playwright.Harness
  ( requireHandle
  , withAssets
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( assertEqual
  , assertFailure
  , testCase
  )

tests :: TestTree
tests =
  testGroup
    "elementhandle-owner-frame"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          setContent page ("<iframe id=frame1 src=\"" <> prefix <> "/empty.html\"></iframe>")
          _ <- waitForFunction page "() => document.querySelector('#frame1')?.contentWindow?.document?.body"
          allFrames <- frames page
          case allFrames of
            _ : child : _ -> do
              jsHandle <- evaluateHandleOnFrame child "() => document.body"
              element <- requireHandle =<< asElement jsHandle
              owner <- ownerFrameHandle element
              case owner of
                Nothing ->
                  assertFailure "expected owner"
                Just frame ->
                  assertEqual "child" True (framesEqual frame child)
            _ ->
              assertFailure "expected child frame"
    , testCase "should work for document" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          jsHandle <- evaluateHandle page "() => document"
          element <- requireHandle =<< asElement jsHandle
          owner <- ownerFrameHandle element
          case owner of
            Nothing ->
              assertFailure "expected owner"
            Just frame ->
              assertEqual "main" True (framesEqual frame (mainFrame page))
    , testCase "should work for detached elements" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          jsHandle <-
            evaluateHandle
              page
              "() => { const div = document.createElement('div'); document.body.appendChild(div); return div; }"
          element <- requireHandle =<< asElement jsHandle
          owner <- ownerFrameHandle element
          case owner of
            Nothing ->
              assertFailure "expected owner"
            Just frame ->
              assertEqual "main" True (framesEqual frame (mainFrame page))
    ]

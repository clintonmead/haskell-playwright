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
  , childFrames
  , evaluateHandleOnFrame
  , goto
  , mainFrame
  , ownerFrameHandle
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
    , testCase "should work for iframe elements" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- attachFrame page "frame1" (prefix <> "/empty.html")
          let mainHandle = mainFrame page
          jsHandle <- evaluateHandleOnFrame mainHandle "() => document.querySelector('#frame1')"
          element <- requireHandle =<< asElement jsHandle
          owner <- ownerFrameHandle element
          case owner of
            Nothing ->
              assertFailure "expected owner"
            Just frame ->
              assertEqual "main" True (framesEqual frame mainHandle)
    , testCase "should work for cross-frame evaluations" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          _ <- attachFrame page "frame1" (prefix <> "/empty.html")
          let mainHandle = mainFrame page
          jsHandle <- evaluateHandleOnFrame mainHandle "() => document.querySelector('iframe').contentWindow.document.body"
          element <- requireHandle =<< asElement jsHandle
          owner <- ownerFrameHandle element
          children <- childFrames mainHandle
          case (owner, children) of
            (Just frame, [child]) -> assertEqual "child" True (framesEqual frame child)
            _ -> assertFailure "expected the owner to be the one child frame"
    ]

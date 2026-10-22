module Playwright.ElementHandleContentFrameSpec
  ( tests
  ) where

import Playwright
  ( asElement
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
    ]

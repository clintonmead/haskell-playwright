module Playwright.FrameFrameElementSpec
  ( tests
  ) where

import Playwright
  ( Frame
  , Page
  , contentFrameHandle
  , defaultGotoOptions
  , elementArgument
  , evaluate
  , evaluateOnElementBoolWith
  , frameElement
  , frames
  , framesEqual
  , getAttributeHandle
  , goto
  , querySelector
  , selectorFromText
  , setContent
  , urlFromText
  , Playwright
  )
import Playwright.Harness
  ( attachFrame
  , assertProtocolContains
  , requireHandle
  , withAssets
  , withChromiumPage
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
    "frame-frame-element"
    [ testCase "should work" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame1 <- attachFrame page "frame1" (prefix <> "/empty.html")
          _ <- attachFrame page "frame2" (prefix <> "/empty.html")
          frame3 <- attachFrame page "frame3" (prefix <> "/empty.html")
          handle1 <- requireHandle =<< querySelector page (selectorFromText "#frame1")
          handle2 <- frameElement frame1
          handle3 <- requireHandle =<< querySelector page (selectorFromText "#frame3")
          handle4 <- frameElement frame3
          assertEqual "frame1 identity" True
            =<< evaluateOnElementBoolWith handle1 "(a, b) => a === b" (elementArgument handle2)
          assertEqual "frame3 identity" True
            =<< evaluateOnElementBoolWith handle3 "(a, b) => a === b" (elementArgument handle4)
          assertEqual "distinct" False
            =<< evaluateOnElementBoolWith handle1 "(a, b) => a === b" (elementArgument handle3)
    , testCase "should work with contentFrame" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame <- attachFrame page "frame1" (prefix <> "/empty.html")
          handle <- frameElement frame
          content <- contentFrameHandle handle
          case content of
            Just actual ->
              assertEqual "same frame" True (framesEqual actual frame)
            Nothing ->
              assertFailure "expected content frame"
    , testCase "should work with frameset" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/frameset.html")) defaultGotoOptions
          handle1 <- requireHandle =<< querySelector page (selectorFromText "frame")
          frame <- requireFrame =<< contentFrameHandle handle1
          handle2 <- frameElement frame
          assertEqual "frameset identity" True
            =<< evaluateOnElementBoolWith handle1 "(a, b) => a === b" (elementArgument handle2)
    , testCase "should throw when detached" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame1 <- attachFrame page "frame1" (prefix <> "/empty.html")
          _ <- evaluate page "() => document.getElementById('frame1').remove()"
          assertProtocolContains "Frame has been detached." (frameElement frame1)
    , testCase "should work inside closed shadow root" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div id=framecontainer></div><script>const iframe = document.createElement('iframe'); iframe.setAttribute('name', 'myframe'); iframe.setAttribute('srcdoc', 'find me'); const div = document.getElementById('framecontainer'); const host = div.attachShadow({ mode: 'closed' }); host.appendChild(iframe);</script>"
          child <- childFrame page
          element <- frameElement child
          assertEqual "name" (Just "myframe") =<< getAttributeHandle element "name"
    , testCase "should work inside declarative shadow root" $
        withChromiumPage $ \page -> do
          setContent
            page
            "<div><template shadowrootmode=\"open\"><iframe name=\"myframe\" srcdoc=\"<h1>Hi!</h1>\"></iframe><slot></slot></template><span>footer</span></div>"
          child <- childFrame page
          element <- frameElement child
          assertEqual "name" (Just "myframe") =<< getAttributeHandle element "name"
    ]

childFrame :: Page -> Playwright Frame
childFrame page = do
  allFrames <- frames page
  case allFrames of
    _ : child : _ ->
      pure child
    _ ->
      assertFailure "expected a child frame"

requireFrame :: Maybe Frame -> Playwright Frame
requireFrame frame =
  case frame of
    Just value ->
      pure value
    Nothing ->
      assertFailure "expected a frame"

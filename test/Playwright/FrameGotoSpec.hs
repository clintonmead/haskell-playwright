module Playwright.FrameGotoSpec
  ( tests
  ) where

import Data.Text
  ( isInfixOf
  )
import Playwright
  ( currentUrl
  , defaultGotoOptions
  , frames
  , framesEqual
  , goto
  , gotoFrame
  , responseFrame
  , responseOk
  , urlFromText
  , urlToText
  )
import Playwright.Harness
  ( withAssets
  , assertBool
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
    "frame-goto"
    [ testCase "should navigate subframes" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          allFrames <- frames page
          case allFrames of
            [parentFrame, childFrame] -> do
              parentUrl <- urlToText <$> currentUrl parentFrame
              childUrl <- urlToText <$> currentUrl childFrame
              assertBool "parent" ("/frames/one-frame.html" `isInfixOf` parentUrl)
              assertBool "child" ("/frames/frame.html" `isInfixOf` childUrl)
              response <-
                gotoFrame childFrame (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
              case response of
                Just value -> do
                  assertEqual "ok" True =<< responseOk value
                  frame <- responseFrame value
                  assertEqual "response frame" True (maybe False (framesEqual childFrame) frame)
                Nothing ->
                  assertFailure "expected navigation response"
            _ ->
              assertFailure ("expected 2 frames, got " <> show (length allFrames))
    ]

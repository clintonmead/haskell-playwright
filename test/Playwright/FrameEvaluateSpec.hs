module Playwright.FrameEvaluateSpec
  ( tests
  ) where

import Data.Text qualified as Text
import Data.Text
  ( isInfixOf
  )
import UnliftIO.Exception
  ( try
  )
import Playwright
  ( Frame
  , Page
  , Playwright
  , asElement
  , childFrames
  , defaultGotoOptions
  , elementArgument
  , evaluate
  , evaluateHandle
  , evaluateHandleOnFrame
  , evaluateOnFrame
  , evaluateOnFrameText
  , evaluateOnFrameWith
  , evaluateTextWith
  , frames
  , goto
  , jsHandleArgument
  , mainFrame
  , setContent
  , urlFromText
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure (protocolFailureMessage)
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertFailure
  , attachFrame
  , requireHandle
  , withAssets
  )
import Playwright.SerializedValue
  ( SerializedValue
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
    , testCase "should not allow cross-frame js handles" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          handle <-
            evaluateHandle
              page
              "() => { const iframe = document.querySelector('iframe'); const foo = { bar: 'baz' }; iframe.contentWindow['__foo'] = foo; return foo; }"
          child <- firstChild page
          assertEqual "child result" "{\"bar\":\"baz\"}"
            =<< evaluateOnFrameText child "() => JSON.stringify(window['__foo'])"
          result <-
            try (evaluateOnFrameWith child "foo => foo.bar" (jsHandleArgument handle))
              :: Playwright (Either PlaywrightError SerializedValue)
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "context"
                ("JSHandles can be evaluated only in the context they were created!" `isInfixOf` protocolFailureMessage failure)
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected the cross-frame handle to be rejected"
    , testCase "should allow cross-frame element handles" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/frames/one-frame.html")) defaultGotoOptions
          child <- firstChild page
          body <- requireHandle =<< asElement =<< evaluateHandleOnFrame child "() => document.body"
          assertEqual "inner html" "<div>Hi, I'm frame</div>" . Text.strip
            =<< evaluateTextWith page "body => body.innerHTML" (elementArgument body)
    , testCase "should throw for detached frames" $
        withAssets $ \prefix page -> do
          _ <- goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions
          frame1 <- attachFrame page "frame1" (prefix <> "/empty.html")
          _ <- evaluate page "() => document.querySelector('iframe').remove()"
          result <-
            try (evaluateOnFrame frame1 "() => 7 * 8")
              :: Playwright (Either PlaywrightError SerializedValue)
          case result of
            Left (ProtocolRpc failure) ->
              assertBool
                "detached"
                ("frame was detached" `isInfixOf` Text.toLower (protocolFailureMessage failure))
            Left err ->
              assertFailure ("expected ProtocolRpc, got: " <> show err)
            Right _ ->
              assertFailure "expected the detached frame to reject"
    ]

-- | The first child frame of the main frame, which must exist.
firstChild :: Page -> Playwright Frame
firstChild page = do
  children <- childFrames (mainFrame page)
  case children of
    child : _ -> pure child
    [] -> assertFailure "expected a child frame"

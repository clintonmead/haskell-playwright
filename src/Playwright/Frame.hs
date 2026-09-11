module Playwright.Frame
  ( Frame (..)
  , frameFromRef
  , gotoFrame
  , titleFrame
  , contentFrame
  , setContentFrame
  , clickFrame
  , fillFrame
  , innerTextFrame
  , innerHtmlFrame
  , textContentFrame
  , getAttributeFrame
  , inputValueFrame
  , isVisibleFrame
  , isHiddenFrame
  , evaluateFrame
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection)
  , FrameTag
  , send
  )
import Playwright.Connection
  ( Connection
  , lookupObject
  )
import Playwright.GotoOptions
  ( GotoOptions (gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( FrameClickParams (frameClickParamsStrict)
  , FrameClickResult
  , FrameContentResult (frameContentResultValue)
  , FrameEvaluateExpressionParams (frameEvaluateExpressionParamsIsFunction)
  , FrameEvaluateExpressionResult (frameEvaluateExpressionResultValue)
  , FrameFillParams (frameFillParamsStrict)
  , FrameFillResult
  , FrameGetAttributeParams (frameGetAttributeParamsStrict)
  , FrameGetAttributeResult (frameGetAttributeResultValue)
  , FrameGotoParams (frameGotoParamsReferer, frameGotoParamsWaitUntil)
  , FrameGotoResult (frameGotoResultResponse)
  , FrameInitializer (frameInitializerParentFrame, frameInitializerUrl)
  , FrameInnerHTMLParams (frameInnerHTMLParamsStrict)
  , FrameInnerHTMLResult (frameInnerHTMLResultValue)
  , FrameInnerTextParams (frameInnerTextParamsStrict)
  , FrameInnerTextResult (frameInnerTextResultValue)
  , FrameInputValueParams (frameInputValueParamsStrict)
  , FrameInputValueResult (frameInputValueResultValue)
  , FrameIsHiddenParams (frameIsHiddenParamsStrict)
  , FrameIsHiddenResult (frameIsHiddenResultValue)
  , FrameIsVisibleParams (frameIsVisibleParamsStrict)
  , FrameIsVisibleResult (frameIsVisibleResultValue)
  , FrameSetContentResult
  , FrameTextContentParams (frameTextContentParamsStrict)
  , FrameTextContentResult (frameTextContentResultValue)
  , FrameTitleResult (frameTitleResultValue)
  , mkFrameClickParams
  , mkFrameContentParams
  , mkFrameEvaluateExpressionParams
  , mkFrameFillParams
  , mkFrameGetAttributeParams
  , mkFrameGotoParams
  , mkFrameInnerHTMLParams
  , mkFrameInnerTextParams
  , mkFrameInputValueParams
  , mkFrameIsHiddenParams
  , mkFrameIsVisibleParams
  , mkFrameSetContentParams
  , mkFrameTextContentParams
  , mkFrameTitleParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Response
  ( Response (Response)
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.SerializedArgument
  ( undefinedArgument
  )
import Playwright.SerializedValue
  ( SerializedValue
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.Url
  ( Url
  )

data Frame = Frame
  { frameChannel :: Channel FrameTag
  , frameUrl :: Url
  , frameParent :: Maybe ChannelRef
  }

frameFromRef :: Connection -> ChannelRef -> IO Frame
frameFromRef conn ref = do
  obj <- lookupObject conn (channelRefGuid ref)
  initializer <- either throwIO pure (fromJsonValue (remoteInitializer obj))
  pure
    Frame
      { frameChannel = Channel conn (channelRefGuid ref)
      , frameUrl = frameInitializerUrl initializer
      , frameParent = frameInitializerParentFrame initializer
      }

gotoFrame :: Frame -> Url -> GotoOptions -> IO (Maybe Response)
gotoFrame frame url opts = do
  result <-
    send
      (frameChannel frame)
      (MethodName "goto")
      (mkFrameGotoParams (gotoOptionsTimeout opts) url)
        { frameGotoParamsWaitUntil = gotoOptionsWaitUntil opts
        , frameGotoParamsReferer = gotoOptionsReferer opts
        }
  case frameGotoResultResponse result of
    Nothing ->
      pure Nothing
    Just responseRef ->
      pure
        ( Just
            ( Response
                ( Channel
                    (channelConnection (frameChannel frame))
                    (channelRefGuid responseRef)
                )
            )
        )

titleFrame :: Frame -> IO Text
titleFrame frame = do
  result <- send (frameChannel frame) (MethodName "title") mkFrameTitleParams
  pure (frameTitleResultValue result)

contentFrame :: Frame -> IO Text
contentFrame frame = do
  result <- send (frameChannel frame) (MethodName "content") mkFrameContentParams
  pure (frameContentResultValue result)

setContentFrame :: Frame -> Text -> Timeout -> IO ()
setContentFrame frame html timeout = do
  _ :: FrameSetContentResult <-
    send (frameChannel frame) (MethodName "setContent") (mkFrameSetContentParams html timeout)
  pure ()

clickFrame :: Frame -> Selector -> Timeout -> IO ()
clickFrame frame selector timeout = do
  _ :: FrameClickResult <-
    send
      (frameChannel frame)
      (MethodName "click")
      (mkFrameClickParams selector timeout)
        { frameClickParamsStrict = Just True
        }
  pure ()

fillFrame :: Frame -> Selector -> Text -> Timeout -> IO ()
fillFrame frame selector value timeout = do
  _ :: FrameFillResult <-
    send
      (frameChannel frame)
      (MethodName "fill")
      (mkFrameFillParams selector timeout value)
        { frameFillParamsStrict = Just True
        }
  pure ()

innerTextFrame :: Frame -> Selector -> Timeout -> IO Text
innerTextFrame frame selector timeout = do
  result <-
    send
      (frameChannel frame)
      (MethodName "innerText")
      (mkFrameInnerTextParams selector timeout)
        { frameInnerTextParamsStrict = Just True
        }
  pure (frameInnerTextResultValue result)

innerHtmlFrame :: Frame -> Selector -> Timeout -> IO Text
innerHtmlFrame frame selector timeout = do
  result <-
    send
      (frameChannel frame)
      (MethodName "innerHTML")
      (mkFrameInnerHTMLParams selector timeout)
        { frameInnerHTMLParamsStrict = Just True
        }
  pure (frameInnerHTMLResultValue result)

textContentFrame :: Frame -> Selector -> Timeout -> IO (Maybe Text)
textContentFrame frame selector timeout = do
  result <-
    send
      (frameChannel frame)
      (MethodName "textContent")
      (mkFrameTextContentParams selector timeout)
        { frameTextContentParamsStrict = Just True
        }
  pure (frameTextContentResultValue result)

getAttributeFrame :: Frame -> Selector -> Text -> Timeout -> IO (Maybe Text)
getAttributeFrame frame selector name timeout = do
  result <-
    send
      (frameChannel frame)
      (MethodName "getAttribute")
      (mkFrameGetAttributeParams name selector timeout)
        { frameGetAttributeParamsStrict = Just True
        }
  pure (frameGetAttributeResultValue result)

inputValueFrame :: Frame -> Selector -> Timeout -> IO Text
inputValueFrame frame selector timeout = do
  result <-
    send
      (frameChannel frame)
      (MethodName "inputValue")
      (mkFrameInputValueParams selector timeout)
        { frameInputValueParamsStrict = Just True
        }
  pure (frameInputValueResultValue result)

isVisibleFrame :: Frame -> Selector -> IO Bool
isVisibleFrame frame selector = do
  result <-
    send
      (frameChannel frame)
      (MethodName "isVisible")
      (mkFrameIsVisibleParams selector)
        { frameIsVisibleParamsStrict = Just True
        }
  pure (frameIsVisibleResultValue result)

isHiddenFrame :: Frame -> Selector -> IO Bool
isHiddenFrame frame selector = do
  result <-
    send
      (frameChannel frame)
      (MethodName "isHidden")
      (mkFrameIsHiddenParams selector)
        { frameIsHiddenParamsStrict = Just True
        }
  pure (frameIsHiddenResultValue result)

evaluateFrame :: Frame -> Text -> IO SerializedValue
evaluateFrame frame expression = do
  result <-
    send
      (frameChannel frame)
      (MethodName "evaluateExpression")
      (mkFrameEvaluateExpressionParams undefinedArgument expression)
        { frameEvaluateExpressionParamsIsFunction = Just True
        }
  pure (frameEvaluateExpressionResultValue result)

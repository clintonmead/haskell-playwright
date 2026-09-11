module Playwright.Frame
  ( Frame (..)
  , evalOnSelectorFrame
  , evaluateFrame
  , frameFromRef
  , frameSend
  , frameSend_
  , contentFrame
  , gotoFrame
  , setContentFrame
  , titleFrame
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
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
  ( FrameContentResult (frameContentResultValue)
  , FrameEvalOnSelectorParams (frameEvalOnSelectorParamsIsFunction, frameEvalOnSelectorParamsStrict)
  , FrameEvalOnSelectorResult (frameEvalOnSelectorResultValue)
  , FrameEvaluateExpressionParams (frameEvaluateExpressionParamsIsFunction)
  , FrameEvaluateExpressionResult (frameEvaluateExpressionResultValue)
  , FrameGotoParams (frameGotoParamsReferer, frameGotoParamsWaitUntil)
  , FrameGotoResult (frameGotoResultResponse)
  , FrameInitializer (frameInitializerParentFrame, frameInitializerUrl)
  , FrameSetContentResult
  , FrameTitleResult (frameTitleResultValue)
  , mkFrameContentParams
  , mkFrameEvalOnSelectorParams
  , mkFrameEvaluateExpressionParams
  , mkFrameGotoParams
  , mkFrameSetContentParams
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
  ( SerializedArgument
  , undefinedArgument
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

frameSend :: forall result params. (ToJSON params, FromJSON result) => Frame -> Text -> params -> IO result
frameSend frame methodName params =
  send (frameChannel frame) (MethodName methodName) params

frameSend_ :: forall result params. (ToJSON params, FromJSON result) => Frame -> Text -> params -> IO ()
frameSend_ frame methodName params = do
  _ <- frameSend @result frame methodName params
  pure ()

gotoFrame :: Frame -> Url -> GotoOptions -> IO (Maybe Response)
gotoFrame frame url opts = do
  result <-
    frameSend
      frame
      "goto"
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
  result <- frameSend frame "title" mkFrameTitleParams
  pure (frameTitleResultValue result)

contentFrame :: Frame -> IO Text
contentFrame frame = do
  result <- frameSend frame "content" mkFrameContentParams
  pure (frameContentResultValue result)

setContentFrame :: Frame -> Text -> Timeout -> IO ()
setContentFrame frame html timeout =
  frameSend_ @FrameSetContentResult frame "setContent" (mkFrameSetContentParams html timeout)

evaluateFrame :: Frame -> Text -> IO SerializedValue
evaluateFrame frame expression = do
  result <-
    frameSend
      frame
      "evaluateExpression"
      (mkFrameEvaluateExpressionParams undefinedArgument expression)
        { frameEvaluateExpressionParamsIsFunction = Just True
        }
  pure (frameEvaluateExpressionResultValue result)

evalOnSelectorFrame :: Frame -> Selector -> Text -> SerializedArgument -> IO SerializedValue
evalOnSelectorFrame frame selector expression arg = do
  result <-
    frameSend
      frame
      "evalOnSelector"
      (mkFrameEvalOnSelectorParams arg expression selector)
        { frameEvalOnSelectorParamsIsFunction = Just True
        , frameEvalOnSelectorParamsStrict = Just True
        }
  pure (frameEvalOnSelectorResultValue result)

module Playwright.Frame
  ( Frame (..)
  , childFrames
  , contentFrame
  , currentName
  , currentUrl
  , evalOnSelectorAllFrame
  , evalOnSelectorFrame
  , evalOnSelectorFrameStrict
  , evaluateFrame
  , evaluateFrameWith
  , frameFromRef
  , framesEqual
  , frameSend
  , frameSend_
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
  ( Channel (Channel, channelConnection, channelGuid)
  , FrameTag
  , send
  )
import Playwright.Connection
  ( Connection
  , guidsOfType
  , lookupContextBaseUrl
  , lookupObject
  )
import Playwright.GotoOptions
  ( GotoOptions (gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( fromJsonValue
  )
import Playwright.LoadState
  ( toLifecycle
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ChannelType (ChannelTypeFrame)
  , FrameContentResult (frameContentResultValue)
  , FrameEvalOnSelectorAllParams (frameEvalOnSelectorAllParamsIsFunction)
  , FrameEvalOnSelectorAllResult (frameEvalOnSelectorAllResultValue)
  , FrameEvalOnSelectorParams (frameEvalOnSelectorParamsIsFunction, frameEvalOnSelectorParamsStrict)
  , FrameEvalOnSelectorResult (frameEvalOnSelectorResultValue)
  , FrameEvaluateExpressionParams (frameEvaluateExpressionParamsIsFunction)
  , FrameEvaluateExpressionResult (frameEvaluateExpressionResultValue)
  , FrameGotoParams (frameGotoParamsReferer, frameGotoParamsWaitUntil)
  , FrameGotoResult (frameGotoResultResponse)
  , FrameInitializer (frameInitializerName, frameInitializerParentFrame, frameInitializerUrl)
  , FrameSetContentResult
  , FrameTitleResult (frameTitleResultValue)
  , mkFrameContentParams
  , mkFrameEvalOnSelectorAllParams
  , mkFrameEvalOnSelectorParams
  , mkFrameEvaluateExpressionParams
  , mkFrameGotoParams
  , mkFrameSetContentParams
  , mkFrameTitleParams
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer, remoteParent)
  )
import Playwright.Response
  ( Response
  , responseFromRef
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
  , resolveAgainstBase
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

currentUrl :: Frame -> IO Url
currentUrl frame = do
  initializer <- frameInitializer frame
  pure (frameInitializerUrl initializer)

currentName :: Frame -> IO Text
currentName frame = do
  initializer <- frameInitializer frame
  pure (frameInitializerName initializer)

frameInitializer :: Frame -> IO FrameInitializer
frameInitializer frame = do
  obj <- lookupObject (channelConnection (frameChannel frame)) (channelGuid (frameChannel frame))
  either throwIO pure (fromJsonValue (remoteInitializer obj))

childFrames :: Frame -> IO [Frame]
childFrames frame = do
  let conn = channelConnection (frameChannel frame)
  guids <- guidsOfType conn ChannelTypeFrame
  candidates <- traverse (\guid -> frameFromRef conn (ChannelRef guid)) guids
  pure (filter (isChildOf frame) candidates)

isChildOf :: Frame -> Frame -> Bool
isChildOf parent child =
  frameParent child == Just (ChannelRef (channelGuid (frameChannel parent)))

resolveFrameUrl :: Frame -> Url -> IO Url
resolveFrameUrl frame url = do
  let conn = channelConnection (frameChannel frame)
  frameObj <- lookupObject conn (channelGuid (frameChannel frame))
  base <- lookupContextBaseUrl conn (remoteParent frameObj)
  pure (resolveAgainstBase base url)

frameSend :: forall result params. (ToJSON params, FromJSON result) => Frame -> Text -> params -> IO result
frameSend frame methodName params =
  send (frameChannel frame) (MethodName methodName) params

frameSend_ :: forall result params. (ToJSON params, FromJSON result) => Frame -> Text -> params -> IO ()
frameSend_ frame methodName params = do
  _ <- frameSend @result frame methodName params
  pure ()

gotoFrame :: Frame -> Url -> GotoOptions -> IO (Maybe Response)
gotoFrame frame url opts = do
  resolved <- resolveFrameUrl frame url
  result <-
    frameSend
      frame
      "goto"
      (mkFrameGotoParams (gotoOptionsTimeout opts) resolved)
        { frameGotoParamsWaitUntil = fmap toLifecycle (gotoOptionsWaitUntil opts)
        , frameGotoParamsReferer = gotoOptionsReferer opts
        }
  case frameGotoResultResponse result of
    Nothing ->
      pure Nothing
    Just responseRef ->
      pure (Just (responseFromRef (channelConnection (frameChannel frame)) responseRef))

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
evaluateFrame frame expression =
  evaluateFrameWith frame expression undefinedArgument

evaluateFrameWith :: Frame -> Text -> SerializedArgument -> IO SerializedValue
evaluateFrameWith frame expression arg = do
  result <-
    frameSend
      frame
      "evaluateExpression"
      (mkFrameEvaluateExpressionParams arg expression)
        { frameEvaluateExpressionParamsIsFunction = Just True
        }
  pure (frameEvaluateExpressionResultValue result)

evalOnSelectorFrame :: Frame -> Selector -> Text -> SerializedArgument -> IO SerializedValue
evalOnSelectorFrame frame selector expression arg =
  evalOnSelectorFrameMaybeStrict frame selector expression arg Nothing

evalOnSelectorFrameStrict :: Frame -> Selector -> Text -> SerializedArgument -> IO SerializedValue
evalOnSelectorFrameStrict frame selector expression arg =
  evalOnSelectorFrameMaybeStrict frame selector expression arg (Just True)

evalOnSelectorFrameMaybeStrict :: Frame -> Selector -> Text -> SerializedArgument -> Maybe Bool -> IO SerializedValue
evalOnSelectorFrameMaybeStrict frame selector expression arg strict = do
  result <-
    frameSend
      frame
      "evalOnSelector"
      (mkFrameEvalOnSelectorParams arg expression selector)
        { frameEvalOnSelectorParamsIsFunction = Just True
        , frameEvalOnSelectorParamsStrict = strict
        }
  pure (frameEvalOnSelectorResultValue result)

framesEqual :: Frame -> Frame -> Bool
framesEqual left right =
  channelGuid (frameChannel left) == channelGuid (frameChannel right)

evalOnSelectorAllFrame :: Frame -> Selector -> Text -> SerializedArgument -> IO SerializedValue
evalOnSelectorAllFrame frame selector expression arg = do
  result <-
    frameSend
      frame
      "evalOnSelectorAll"
      (mkFrameEvalOnSelectorAllParams arg expression selector)
        { frameEvalOnSelectorAllParamsIsFunction = Just True
        }
  pure (frameEvalOnSelectorAllResultValue result)

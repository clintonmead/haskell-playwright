module Playwright.Frame
  ( Frame (..)
  , childFrames
  , frameContent
  , currentName
  , currentUrl
  , parentFrame
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
  , setContentFrameWith
  , setContentOnFrame
  , titleFrame
  ) where

import Playwright.Monad
  ( Playwright
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , FrameTag
  , send
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.Connection
  ( guidsOfType
  , lookupContextBaseUrl
  , lookupMainFramePage
  , lookupObject
  , lookupTimeoutDefaults
  )
import Playwright.GotoOptions
  ( GotoOptions (gotoOptionsReferer, gotoOptionsTimeout, gotoOptionsWaitUntil)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.LoadState
  ( LoadState
  , defaultNavigationLoadState
  , toLifecycle
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
  , FrameSetContentParams (frameSetContentParamsWaitUntil)
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
  , defaultTimeout
  )
import Playwright.TimeoutDefaults
  ( emptyTimeoutDefaults
  , resolveNavigationTimeout
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

frameFromRef :: ChannelRef -> Playwright Frame
frameFromRef ref = do
  obj <- lookupObject (channelRefGuid ref)
  initializer <- throwDecode (remoteInitializer obj)
  pure
    Frame
      { frameChannel = Channel (channelRefGuid ref)
      , frameUrl = frameInitializerUrl initializer
      , frameParent = frameInitializerParentFrame initializer
      }

currentUrl :: Frame -> Playwright Url
currentUrl frame = do
  initializer <- frameInitializer frame
  pure (frameInitializerUrl initializer)

currentName :: Frame -> Playwright Text
currentName frame = do
  initializer <- frameInitializer frame
  pure (frameInitializerName initializer)

-- | The frame that contains this one. The main frame has none.
parentFrame :: Frame -> Playwright (Maybe Frame)
parentFrame frame =
  case frameParent frame of
    Nothing ->
      pure Nothing
    Just parent ->
      Just <$> frameFromRef parent

frameInitializer :: Frame -> Playwright FrameInitializer
frameInitializer frame = do
  obj <- lookupObject (channelGuid (frameChannel frame))
  throwDecode (remoteInitializer obj)

childFrames :: Frame -> Playwright [Frame]
childFrames frame = do
  guids <- guidsOfType ChannelTypeFrame
  candidates <- traverse (\guid -> frameFromRef (ChannelRef guid)) guids
  pure (filter (isChildOf frame) candidates)

isChildOf :: Frame -> Frame -> Bool
isChildOf parent child =
  frameParent child == Just (ChannelRef (channelGuid (frameChannel parent)))

resolveFrameUrl :: Frame -> Url -> Playwright Url
resolveFrameUrl frame url = do
  frameObj <- lookupObject (channelGuid (frameChannel frame))
  base <- lookupContextBaseUrl (remoteParent frameObj)
  pure (resolveAgainstBase base url)

frameSend :: forall result params. (ToJSON params, FromJSON result) => Frame -> Text -> params -> Playwright result
frameSend frame methodName params =
  send (frameChannel frame) (MethodName methodName) params

frameSend_ :: forall result params. (ToJSON params, FromJSON result) => Frame -> Text -> params -> Playwright ()
frameSend_ frame methodName params = do
  _ <- frameSend @result frame methodName params
  pure ()

gotoFrame :: Frame -> Url -> GotoOptions -> Playwright (Maybe Response)
gotoFrame frame url opts = do
  resolved <- resolveFrameUrl frame url
  timeout <- resolveFrameNavigationTimeout frame (gotoOptionsTimeout opts)
  result <-
    frameSend
      frame
      "goto"
      (mkFrameGotoParams timeout resolved)
        { frameGotoParamsWaitUntil = Just (toLifecycle (fromMaybe defaultNavigationLoadState (gotoOptionsWaitUntil opts)))
        , frameGotoParamsReferer = gotoOptionsReferer opts
        }
  case frameGotoResultResponse result of
    Nothing ->
      pure Nothing
    Just responseRef ->
      pure (Just (responseFromRef responseRef))

resolveFrameNavigationTimeout :: Frame -> Maybe Timeout -> Playwright Timeout
resolveFrameNavigationTimeout frame explicit =
  case explicit of
    Just timeout ->
      pure timeout
    Nothing ->
      frameNavigationTimeout frame

frameNavigationTimeout :: Frame -> Playwright Timeout
frameNavigationTimeout frame = do
  mainGuid <- mainFrameGuid frame
  pageGuid <- lookupMainFramePage mainGuid
  case pageGuid of
    Just page -> do
      pageObj <- lookupObject page
      pageDefaults <- lookupTimeoutDefaults page
      contextDefaults <- lookupTimeoutDefaults (remoteParent pageObj)
      pure (resolveNavigationTimeout pageDefaults contextDefaults)
    Nothing -> do
      frameObj <- lookupObject (channelGuid (frameChannel frame))
      contextDefaults <- lookupTimeoutDefaults (remoteParent frameObj)
      pure (resolveNavigationTimeout emptyTimeoutDefaults contextDefaults)

mainFrameGuid :: Frame -> Playwright Guid
mainFrameGuid frame =
  case frameParent frame of
    Nothing ->
      pure (channelGuid (frameChannel frame))
    Just parent -> do
      containing <- frameFromRef parent
      mainFrameGuid containing

titleFrame :: Frame -> Playwright Text
titleFrame frame = do
  result <- frameSend frame "title" mkFrameTitleParams
  pure (frameTitleResultValue result)

frameContent :: Frame -> Playwright Text
frameContent frame = do
  result <- frameSend frame "content" mkFrameContentParams
  pure (frameContentResultValue result)

setContentFrame :: Frame -> Text -> Timeout -> Playwright ()
setContentFrame frame html timeout =
  setContentFrameWith frame html timeout Nothing

setContentFrameWith :: Frame -> Text -> Timeout -> Maybe LoadState -> Playwright ()
setContentFrameWith frame html timeout waitUntil =
  frameSend_ @FrameSetContentResult
    frame
    "setContent"
    (mkFrameSetContentParams html timeout)
      { frameSetContentParamsWaitUntil = fmap toLifecycle waitUntil
      }

setContentOnFrame :: Frame -> Text -> Playwright ()
setContentOnFrame frame html =
  setContentFrame frame html defaultTimeout

evaluateFrame :: Frame -> Text -> Playwright SerializedValue
evaluateFrame frame expression =
  evaluateFrameWith frame expression undefinedArgument

evaluateFrameWith :: Frame -> Text -> SerializedArgument -> Playwright SerializedValue
evaluateFrameWith frame expression arg = do
  result <-
    frameSend
      frame
      "evaluateExpression"
      (mkFrameEvaluateExpressionParams arg expression)
        { frameEvaluateExpressionParamsIsFunction = Just True
        }
  pure (frameEvaluateExpressionResultValue result)

evalOnSelectorFrame :: Frame -> Selector -> Text -> SerializedArgument -> Playwright SerializedValue
evalOnSelectorFrame frame selector expression arg =
  evalOnSelectorFrameMaybeStrict frame selector expression arg Nothing

evalOnSelectorFrameStrict :: Frame -> Selector -> Text -> SerializedArgument -> Playwright SerializedValue
evalOnSelectorFrameStrict frame selector expression arg =
  evalOnSelectorFrameMaybeStrict frame selector expression arg (Just True)

evalOnSelectorFrameMaybeStrict :: Frame -> Selector -> Text -> SerializedArgument -> Maybe Bool -> Playwright SerializedValue
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

evalOnSelectorAllFrame :: Frame -> Selector -> Text -> SerializedArgument -> Playwright SerializedValue
evalOnSelectorAllFrame frame selector expression arg = do
  result <-
    frameSend
      frame
      "evalOnSelectorAll"
      (mkFrameEvalOnSelectorAllParams arg expression selector)
        { frameEvalOnSelectorAllParamsIsFunction = Just True
        }
  pure (frameEvalOnSelectorAllResultValue result)

module Playwright.Frame.Wait
  ( waitForFunctionFrame
  , waitForLoadStateFrame
  , waitForSelectorFrame
  , waitForTimeoutFrame
  ) where

import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (channelConnection, channelGuid)
  )
import Playwright.Connection
  ( waitForLifecycle
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , frameSend
  , frameSend_
  )
import Playwright.JsHandle
  ( JsHandle
  , jsHandleFromRef
  )
import Playwright.LoadState
  ( LoadState
  , toLifecycle
  )
import Playwright.Protocol
  ( FrameWaitForFunctionParams (frameWaitForFunctionParamsIsFunction)
  , FrameWaitForFunctionResult (frameWaitForFunctionResultHandle)
  , FrameWaitForSelectorParams (frameWaitForSelectorParamsOmitReturnValue, frameWaitForSelectorParamsState, frameWaitForSelectorParamsStrict)
  , FrameWaitForSelectorParamsState (FrameWaitForSelectorParamsStateAttached, FrameWaitForSelectorParamsStateDetached, FrameWaitForSelectorParamsStateHidden, FrameWaitForSelectorParamsStateVisible)
  , FrameWaitForSelectorResult
  , FrameWaitForTimeoutResult
  , mkFrameWaitForFunctionParams
  , mkFrameWaitForSelectorParams
  , mkFrameWaitForTimeoutParams
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.WaitForState
  ( WaitForState (WaitForAttached, WaitForDetached, WaitForHidden, WaitForVisible)
  )

waitForSelectorFrame :: Frame -> Selector -> WaitForState -> Timeout -> Maybe Bool -> IO ()
waitForSelectorFrame frame selector state timeout strict = do
  _ :: FrameWaitForSelectorResult <-
    frameSend
      frame
      "waitForSelector"
      (mkFrameWaitForSelectorParams selector timeout)
        { frameWaitForSelectorParamsStrict = strict
        , frameWaitForSelectorParamsOmitReturnValue = Just True
        , frameWaitForSelectorParamsState = Just (toProtocolState state)
        }
  pure ()

waitForTimeoutFrame :: Frame -> Timeout -> IO ()
waitForTimeoutFrame frame timeout =
  frameSend_ @FrameWaitForTimeoutResult
    frame
    "waitForTimeout"
    (mkFrameWaitForTimeoutParams timeout)

waitForFunctionFrame :: Frame -> Text -> SerializedArgument -> Timeout -> IO JsHandle
waitForFunctionFrame frame expression arg timeout = do
  result <-
    frameSend
      frame
      "waitForFunction"
      (mkFrameWaitForFunctionParams arg expression timeout)
        { frameWaitForFunctionParamsIsFunction = Just True
        }
  pure
    ( jsHandleFromRef
        (channelConnection (frameChannel frame))
        (frameWaitForFunctionResultHandle result)
    )

waitForLoadStateFrame :: Frame -> LoadState -> IO ()
waitForLoadStateFrame frame state =
  waitForLifecycle
    (channelConnection (frameChannel frame))
    (channelGuid (frameChannel frame))
    (toLifecycle state)

toProtocolState :: WaitForState -> FrameWaitForSelectorParamsState
toProtocolState state =
  case state of
    WaitForAttached ->
      FrameWaitForSelectorParamsStateAttached
    WaitForDetached ->
      FrameWaitForSelectorParamsStateDetached
    WaitForVisible ->
      FrameWaitForSelectorParamsStateVisible
    WaitForHidden ->
      FrameWaitForSelectorParamsStateHidden

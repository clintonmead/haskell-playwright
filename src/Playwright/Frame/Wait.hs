module Playwright.Frame.Wait
  ( waitForFunctionFrame
  , waitForLoadStateFrame
  , waitForSelectorFrame
  , waitForSelectorHandleFrame
  , waitForSelectorHandleOnFrame
  , waitForTimeoutFrame
  ) where

import Playwright.Monad
  ( Playwright
  )
import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( Channel (channelGuid)
  )
import Playwright.Connection
  ( waitForLifecycle
  )
import Playwright.ElementHandle
  ( ElementHandle
  , elementHandleFromRef
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
  ( FrameWaitForFunctionParams (frameWaitForFunctionParamsIsFunction, frameWaitForFunctionParamsPollingInterval)
  , FrameWaitForFunctionResult (frameWaitForFunctionResultHandle)
  , FrameWaitForSelectorParams (frameWaitForSelectorParamsOmitReturnValue, frameWaitForSelectorParamsState, frameWaitForSelectorParamsStrict)
  , FrameWaitForSelectorParamsState (FrameWaitForSelectorParamsStateAttached, FrameWaitForSelectorParamsStateDetached, FrameWaitForSelectorParamsStateHidden, FrameWaitForSelectorParamsStateVisible)
  , FrameWaitForSelectorResult (frameWaitForSelectorResultElement)
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
  , defaultTimeout
  )
import Playwright.WaitForState
  ( WaitForState (WaitForAttached, WaitForDetached, WaitForHidden, WaitForVisible)
  )

waitForSelectorFrame :: Frame -> Selector -> WaitForState -> Timeout -> Maybe Bool -> Playwright ()
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

waitForSelectorHandleFrame :: Frame -> Selector -> WaitForState -> Timeout -> Maybe Bool -> Playwright (Maybe ElementHandle)
waitForSelectorHandleFrame frame selector state timeout strict = do
  result <-
    frameSend
      frame
      "waitForSelector"
      (mkFrameWaitForSelectorParams selector timeout)
        { frameWaitForSelectorParamsStrict = strict
        , frameWaitForSelectorParamsOmitReturnValue = Nothing
        , frameWaitForSelectorParamsState = Just (toProtocolState state)
        }
  pure
    ( fmap
        (elementHandleFromRef)
        (frameWaitForSelectorResultElement result)
    )

waitForSelectorHandleOnFrame :: Frame -> Selector -> WaitForState -> Playwright (Maybe ElementHandle)
waitForSelectorHandleOnFrame frame selector state =
  waitForSelectorHandleFrame frame selector state defaultTimeout Nothing

waitForTimeoutFrame :: Frame -> Timeout -> Playwright ()
waitForTimeoutFrame frame timeout =
  frameSend_ @FrameWaitForTimeoutResult
    frame
    "waitForTimeout"
    (mkFrameWaitForTimeoutParams timeout)

waitForFunctionFrame :: Frame -> Text -> SerializedArgument -> Timeout -> Maybe Scientific -> Playwright JsHandle
waitForFunctionFrame frame expression arg timeout polling = do
  result <-
    frameSend
      frame
      "waitForFunction"
      (mkFrameWaitForFunctionParams arg expression timeout)
        { frameWaitForFunctionParamsIsFunction = Just True
        , frameWaitForFunctionParamsPollingInterval = polling
        }
  pure
    ( jsHandleFromRef
        (frameWaitForFunctionResultHandle result)
    )

waitForLoadStateFrame :: Frame -> LoadState -> Playwright ()
waitForLoadStateFrame frame state =
  waitForLifecycle
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

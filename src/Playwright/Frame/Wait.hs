module Playwright.Frame.Wait
  ( waitForSelectorFrame
  ) where

import Playwright.Frame
  ( Frame
  , frameSend
  )
import Playwright.Protocol
  ( FrameWaitForSelectorParams (frameWaitForSelectorParamsOmitReturnValue, frameWaitForSelectorParamsState, frameWaitForSelectorParamsStrict)
  , FrameWaitForSelectorParamsState (FrameWaitForSelectorParamsStateAttached, FrameWaitForSelectorParamsStateDetached, FrameWaitForSelectorParamsStateHidden, FrameWaitForSelectorParamsStateVisible)
  , FrameWaitForSelectorResult
  , mkFrameWaitForSelectorParams
  )
import Playwright.Selector
  ( Selector
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

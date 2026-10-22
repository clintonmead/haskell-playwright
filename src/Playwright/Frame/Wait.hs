module Playwright.Frame.Wait
  ( waitForFunctionFrame
  , waitForLoadStateFrame
  , waitForSelectorFrame
  , waitForSelectorHandleFrame
  , waitForSelectorHandleOnFrame
  , waitForTimeoutFrame
  , waitForURLFrame
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
import Control.Monad.IO.Class
  ( liftIO
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import UnliftIO.Exception
  ( throwIO
  )
import Playwright.Channel
  ( Channel (channelGuid)
  )
import Playwright.Connection
  ( waitForLifecycle
  , waitUntilUrl
  )
import Playwright.ElementHandle
  ( ElementHandle
  , elementHandleFromRef
  )
import Playwright.Frame
  ( Frame (frameChannel)
  , frameSend_
  , frameSendWithTimeout
  )
import Playwright.JsHandle
  ( JsHandle
  , jsHandleFromRef
  )
import Playwright.LoadState
  ( LoadState (LoadStateLoad)
  , toLifecycle
  )
import Playwright.RaceTimeout
  ( waitWithTimeout
  )
import Playwright.UrlMatch
  ( UrlMatch
  , matchesUrlMatch
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
    frameSendWithTimeout
      frame
      "waitForSelector"
      timeout
      (mkFrameWaitForSelectorParams selector)
        { frameWaitForSelectorParamsStrict = strict
        , frameWaitForSelectorParamsOmitReturnValue = Just True
        , frameWaitForSelectorParamsState = Just (toProtocolState state)
        }
  pure ()

waitForSelectorHandleFrame :: Frame -> Selector -> WaitForState -> Timeout -> Maybe Bool -> Playwright (Maybe ElementHandle)
waitForSelectorHandleFrame frame selector state timeout strict = do
  result <-
    frameSendWithTimeout
      frame
      "waitForSelector"
      timeout
      (mkFrameWaitForSelectorParams selector)
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
    frameSendWithTimeout
      frame
      "waitForFunction"
      timeout
      (mkFrameWaitForFunctionParams arg expression)
        { frameWaitForFunctionParamsIsFunction = Just True
        , frameWaitForFunctionParamsPollingInterval = polling
        }
  case frameWaitForFunctionResultHandle result of
    Just handleRef ->
      pure (jsHandleFromRef handleRef)
    Nothing ->
      liftIO (throwIO (DecodeFailure "waitForFunction returned no handle"))

waitForLoadStateFrame :: Frame -> LoadState -> Playwright ()
waitForLoadStateFrame frame state =
  waitForLifecycle
    (channelGuid (frameChannel frame))
    (toLifecycle state)

-- | Waits for the frame's URL to match, then for its load event. The timeout is
-- the default one, since a frame has no page navigation timeout to read.
waitForURLFrame :: Frame -> UrlMatch -> Playwright ()
waitForURLFrame frame glob =
  waitWithTimeout defaultTimeout $ do
    waitUntilUrl
      (channelGuid (frameChannel frame))
      (matchesUrlMatch glob)
    waitForLoadStateFrame frame LoadStateLoad

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

module Playwright.Frame.AriaSnapshot
  ( frameAriaSnapshot
  , frameAriaSnapshotJSON
  ) where

import Playwright.AriaSnapshotMode
  ( AriaSnapshotMode (AriaSnapshotModeAi, AriaSnapshotModeDefault)
  )
import Playwright.AriaSnapshotOptions
  ( AriaSnapshotOptions (ariaSnapshotOptionsBoxes, ariaSnapshotOptionsDepth, ariaSnapshotOptionsMode)
  )
import Playwright.Frame
  ( Frame
  , frameSendWithTimeout
  )
import Playwright.Json.OpenJson
  ( OpenJson
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Protocol
  ( FrameAriaSnapshotJSONParamsMode (FrameAriaSnapshotJSONParamsModeAi, FrameAriaSnapshotJSONParamsModeDefault)
  , FrameAriaSnapshotJSONResult (frameAriaSnapshotJSONResultSnapshot)
  , FrameAriaSnapshotParamsMode (FrameAriaSnapshotParamsModeAi, FrameAriaSnapshotParamsModeDefault)
  , FrameAriaSnapshotResult (frameAriaSnapshotResultSnapshot)
  , mkFrameAriaSnapshotJSONParams
  , mkFrameAriaSnapshotParams
  , FrameAriaSnapshotJSONParams (frameAriaSnapshotJSONParamsBoxes, frameAriaSnapshotJSONParamsDepth, frameAriaSnapshotJSONParamsMode, frameAriaSnapshotJSONParamsSelector)
  , FrameAriaSnapshotParams (frameAriaSnapshotParamsBoxes, frameAriaSnapshotParamsDepth, frameAriaSnapshotParamsMode, frameAriaSnapshotParamsSelector)
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.Timeout
  ( Timeout
  )
import Data.Text
  ( Text
  )

-- | Aria snapshot of the frame, or of the element matched by the selector.
frameAriaSnapshot :: Frame -> Maybe Selector -> Timeout -> AriaSnapshotOptions -> Playwright Text
frameAriaSnapshot frame selector timeout opts = do
  result <-
    frameSendWithTimeout
      frame
      "ariaSnapshot"
      timeout
      ( mkFrameAriaSnapshotParams
          { frameAriaSnapshotParamsMode = fmap toParamsMode (ariaSnapshotOptionsMode opts)
          , frameAriaSnapshotParamsSelector = selector
          , frameAriaSnapshotParamsDepth = ariaSnapshotOptionsDepth opts
          , frameAriaSnapshotParamsBoxes = ariaSnapshotOptionsBoxes opts
          }
      )
  pure (frameAriaSnapshotResultSnapshot result)

-- | JSON form of the aria snapshot.
frameAriaSnapshotJSON :: Frame -> Maybe Selector -> Timeout -> AriaSnapshotOptions -> Playwright OpenJson
frameAriaSnapshotJSON frame selector timeout opts = do
  result <-
    frameSendWithTimeout
      frame
      "ariaSnapshotJSON"
      timeout
      ( mkFrameAriaSnapshotJSONParams
          { frameAriaSnapshotJSONParamsMode = fmap toJsonParamsMode (ariaSnapshotOptionsMode opts)
          , frameAriaSnapshotJSONParamsSelector = selector
          , frameAriaSnapshotJSONParamsDepth = ariaSnapshotOptionsDepth opts
          , frameAriaSnapshotJSONParamsBoxes = ariaSnapshotOptionsBoxes opts
          }
      )
  pure (frameAriaSnapshotJSONResultSnapshot result)

toParamsMode :: AriaSnapshotMode -> FrameAriaSnapshotParamsMode
toParamsMode mode =
  case mode of
    AriaSnapshotModeAi -> FrameAriaSnapshotParamsModeAi
    AriaSnapshotModeDefault -> FrameAriaSnapshotParamsModeDefault

toJsonParamsMode :: AriaSnapshotMode -> FrameAriaSnapshotJSONParamsMode
toJsonParamsMode mode =
  case mode of
    AriaSnapshotModeAi -> FrameAriaSnapshotJSONParamsModeAi
    AriaSnapshotModeDefault -> FrameAriaSnapshotJSONParamsModeDefault

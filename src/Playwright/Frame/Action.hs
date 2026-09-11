module Playwright.Frame.Action
  ( checkFrame
  , clickFrame
  , dblclickFrame
  , fillFrame
  , focusFrame
  , hoverFrame
  , pressFrame
  , typeTextFrame
  , uncheckFrame
  ) where

import Data.Text
  ( Text
  )
import Playwright.CheckOptions
  ( CheckOptions (checkOptionsForce, checkOptionsTimeout, checkOptionsTrial)
  )
import Playwright.Frame
  ( Frame
  , frameSend_
  )
import Playwright.Key
  ( Key (keyToText)
  )
import Playwright.Protocol
  ( FrameCheckParams (frameCheckParamsForce, frameCheckParamsStrict, frameCheckParamsTrial)
  , FrameCheckResult
  , FrameClickParams (frameClickParamsStrict)
  , FrameClickResult
  , FrameDblclickParams (frameDblclickParamsStrict)
  , FrameDblclickResult
  , FrameFillParams (frameFillParamsStrict)
  , FrameFillResult
  , FrameFocusParams (frameFocusParamsStrict)
  , FrameFocusResult
  , FrameHoverParams (frameHoverParamsStrict)
  , FrameHoverResult
  , FramePressParams (framePressParamsStrict)
  , FramePressResult
  , FrameTypeParams (frameTypeParamsStrict)
  , FrameTypeResult
  , FrameUncheckParams (frameUncheckParamsForce, frameUncheckParamsStrict, frameUncheckParamsTrial)
  , FrameUncheckResult
  , mkFrameCheckParams
  , mkFrameClickParams
  , mkFrameDblclickParams
  , mkFrameFillParams
  , mkFrameFocusParams
  , mkFrameHoverParams
  , mkFramePressParams
  , mkFrameTypeParams
  , mkFrameUncheckParams
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.Timeout
  ( Timeout
  )

clickFrame :: Frame -> Selector -> Timeout -> IO ()
clickFrame frame selector timeout =
  frameSend_ @FrameClickResult
    frame
    "click"
    (mkFrameClickParams selector timeout)
      { frameClickParamsStrict = Just True
      }

dblclickFrame :: Frame -> Selector -> Timeout -> IO ()
dblclickFrame frame selector timeout =
  frameSend_ @FrameDblclickResult
    frame
    "dblclick"
    (mkFrameDblclickParams selector timeout)
      { frameDblclickParamsStrict = Just True
      }

fillFrame :: Frame -> Selector -> Text -> Timeout -> IO ()
fillFrame frame selector value timeout =
  frameSend_ @FrameFillResult
    frame
    "fill"
    (mkFrameFillParams selector timeout value)
      { frameFillParamsStrict = Just True
      }

checkFrame :: Frame -> Selector -> CheckOptions -> IO ()
checkFrame frame selector opts =
  frameSend_ @FrameCheckResult
    frame
    "check"
    (mkFrameCheckParams selector (checkOptionsTimeout opts))
      { frameCheckParamsStrict = Just True
      , frameCheckParamsForce = checkOptionsForce opts
      , frameCheckParamsTrial = checkOptionsTrial opts
      }

uncheckFrame :: Frame -> Selector -> CheckOptions -> IO ()
uncheckFrame frame selector opts =
  frameSend_ @FrameUncheckResult
    frame
    "uncheck"
    (mkFrameUncheckParams selector (checkOptionsTimeout opts))
      { frameUncheckParamsStrict = Just True
      , frameUncheckParamsForce = checkOptionsForce opts
      , frameUncheckParamsTrial = checkOptionsTrial opts
      }

focusFrame :: Frame -> Selector -> Timeout -> IO ()
focusFrame frame selector timeout =
  frameSend_ @FrameFocusResult
    frame
    "focus"
    (mkFrameFocusParams selector timeout)
      { frameFocusParamsStrict = Just True
      }

hoverFrame :: Frame -> Selector -> Timeout -> IO ()
hoverFrame frame selector timeout =
  frameSend_ @FrameHoverResult
    frame
    "hover"
    (mkFrameHoverParams selector timeout)
      { frameHoverParamsStrict = Just True
      }

pressFrame :: Frame -> Selector -> Key -> Timeout -> IO ()
pressFrame frame selector key timeout =
  frameSend_ @FramePressResult
    frame
    "press"
    (mkFramePressParams (keyToText key) selector timeout)
      { framePressParamsStrict = Just True
      }

typeTextFrame :: Frame -> Selector -> Text -> Timeout -> IO ()
typeTextFrame frame selector text timeout =
  frameSend_ @FrameTypeResult
    frame
    "type"
    (mkFrameTypeParams selector text timeout)
      { frameTypeParamsStrict = Just True
      }

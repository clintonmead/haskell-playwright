module Playwright.Frame.Action
  ( checkFrame
  , clickFrame
  , dblclickFrame
  , dispatchEventFrame
  , fillFrame
  , focusFrame
  , hoverFrame
  , pressFrame
  , selectOptionFrame
  , tapFrame
  , typeTextFrame
  , uncheckFrame
  , dragAndDropFrame
  , setInputFilesFrame
  ) where

import Data.Text
  ( Text
  )
import Playwright.CheckOptions
  ( CheckOptions (checkOptionsForce, checkOptionsTimeout, checkOptionsTrial)
  )
import Playwright.DomEventType
  ( DomEventType (domEventTypeToText)
  )
import Playwright.Frame
  ( Frame
  , frameSend
  , frameSend_
  )
import Playwright.InputFile
  ( InputFile (inputFileBytes, inputFileName)
  )
import Playwright.InputFileName
  ( inputFileNameToText
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryFromByteString
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
  , FrameDispatchEventParams (frameDispatchEventParamsStrict)
  , FrameDispatchEventResult
  , FrameDragAndDropParams (frameDragAndDropParamsStrict)
  , FrameDragAndDropResult
  , FrameFillParams (frameFillParamsStrict)
  , FrameFillResult
  , FrameFocusParams (frameFocusParamsStrict)
  , FrameFocusResult
  , FrameHoverParams (frameHoverParamsStrict)
  , FrameHoverResult
  , FramePressParams (framePressParamsStrict)
  , FramePressResult
  , FrameSelectOptionParams (frameSelectOptionParamsOptions, frameSelectOptionParamsStrict)
  , FrameSelectOptionParamsOptions (frameSelectOptionParamsOptionsIndex, frameSelectOptionParamsOptionsLabel, frameSelectOptionParamsOptionsValue, frameSelectOptionParamsOptionsValueOrLabel)
  , FrameSelectOptionResult (frameSelectOptionResultValues)
  , FrameSetInputFilesParams (frameSetInputFilesParamsPayloads, frameSetInputFilesParamsStrict)
  , FrameSetInputFilesParamsPayloads
  , FrameSetInputFilesResult
  , FrameTapParams (frameTapParamsStrict)
  , FrameTapResult
  , FrameTypeParams (frameTypeParamsStrict)
  , FrameTypeResult
  , FrameUncheckParams (frameUncheckParamsForce, frameUncheckParamsStrict, frameUncheckParamsTrial)
  , FrameUncheckResult
  , mkFrameCheckParams
  , mkFrameClickParams
  , mkFrameDblclickParams
  , mkFrameDispatchEventParams
  , mkFrameDragAndDropParams
  , mkFrameFillParams
  , mkFrameFocusParams
  , mkFrameHoverParams
  , mkFramePressParams
  , mkFrameSelectOptionParams
  , mkFrameSelectOptionParamsOptions
  , mkFrameSetInputFilesParams
  , mkFrameSetInputFilesParamsPayloads
  , mkFrameTapParams
  , mkFrameTypeParams
  , mkFrameUncheckParams
  )
import Playwright.SerializedArgument
  ( undefinedArgument
  )
import Playwright.SelectOption
  ( SelectOption (SelectIndex, SelectLabel, SelectValue, SelectValueOrLabel)
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

dispatchEventFrame :: Frame -> Selector -> DomEventType -> Timeout -> IO ()
dispatchEventFrame frame selector eventType timeout =
  frameSend_ @FrameDispatchEventResult
    frame
    "dispatchEvent"
    (mkFrameDispatchEventParams undefinedArgument selector timeout (domEventTypeToText eventType))
      { frameDispatchEventParamsStrict = Just True
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

tapFrame :: Frame -> Selector -> Timeout -> IO ()
tapFrame frame selector timeout =
  frameSend_ @FrameTapResult
    frame
    "tap"
    (mkFrameTapParams selector timeout)
      { frameTapParamsStrict = Just True
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

dragAndDropFrame :: Frame -> Selector -> Selector -> Timeout -> IO ()
dragAndDropFrame frame source target timeout =
  frameSend_ @FrameDragAndDropResult
    frame
    "dragAndDrop"
    (mkFrameDragAndDropParams source target timeout)
      { frameDragAndDropParamsStrict = Just True
      }

setInputFilesFrame :: Frame -> Selector -> [InputFile] -> Timeout -> IO ()
setInputFilesFrame frame selector files timeout =
  frameSend_ @FrameSetInputFilesResult
    frame
    "setInputFiles"
    (mkFrameSetInputFilesParams selector timeout)
      { frameSetInputFilesParamsStrict = Just True
      , frameSetInputFilesParamsPayloads = Just (fmap toPayload files)
      }

toPayload :: InputFile -> FrameSetInputFilesParamsPayloads
toPayload file =
  mkFrameSetInputFilesParamsPayloads
    (protocolBinaryFromByteString (inputFileBytes file))
    (inputFileNameToText (inputFileName file))

selectOptionFrame :: Frame -> Selector -> [SelectOption] -> Timeout -> IO [Text]
selectOptionFrame frame selector options timeout = do
  result <-
    frameSend
      frame
      "selectOption"
      (mkFrameSelectOptionParams selector timeout)
        { frameSelectOptionParamsStrict = Just True
        , frameSelectOptionParamsOptions = Just (fmap toProtocolOption options)
        }
  pure (frameSelectOptionResultValues result)

toProtocolOption :: SelectOption -> FrameSelectOptionParamsOptions
toProtocolOption option =
  case option of
    SelectValueOrLabel text ->
      mkFrameSelectOptionParamsOptions {frameSelectOptionParamsOptionsValueOrLabel = Just text}
    SelectValue text ->
      mkFrameSelectOptionParamsOptions {frameSelectOptionParamsOptionsValue = Just text}
    SelectLabel text ->
      mkFrameSelectOptionParamsOptions {frameSelectOptionParamsOptionsLabel = Just text}
    SelectIndex index ->
      mkFrameSelectOptionParamsOptions {frameSelectOptionParamsOptionsIndex = Just index}

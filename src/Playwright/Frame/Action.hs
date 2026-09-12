module Playwright.Frame.Action
  ( blurFrame
  , checkFrame
  , clickFrame
  , clickFrameWith
  , dblclickFrame
  , dispatchEventFrame
  , fillFrame
  , focusFrame
  , hoverFrame
  , highlightFrame
  , hideHighlightFrame
  , pressFrame
  , selectOptionFrame
  , tapFrame
  , typeTextFrame
  , uncheckFrame
  , dragAndDropFrame
  , dropFrame
  , setInputFilesFrame
  ) where

import Control.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.CheckOptions
  ( CheckOptions (checkOptionsForce, checkOptionsTimeout, checkOptionsTrial)
  )
import Playwright.ClickOptions
  ( ClickOptions (clickOptionsForce, clickOptionsNoWaitAfter, clickOptionsPosition)
  , defaultClickOptions
  )
import Playwright.CssPixel
  ( cssPixelToScientific
  )
import Playwright.DomEventType
  ( DomEventType (domEventTypeToText)
  )
import Playwright.DropPayload
  ( DropData (dropDataMimeType, dropDataValue)
  , DropFile (dropFileBytes, dropFileMimeType, dropFileName)
  , DropPayload (dropPayloadData, dropPayloadFiles)
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
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
import Playwright.MimeType
  ( mimeTypeToText
  )
import Playwright.Point
  ( Point
  , pointX
  , pointY
  )
import Playwright.Protocol
  ( FrameBlurParams (frameBlurParamsStrict)
  , FrameBlurResult
  , FrameCheckParams (frameCheckParamsForce, frameCheckParamsStrict, frameCheckParamsTrial)
  , FrameCheckResult
  , FrameClickParams (frameClickParamsForce, frameClickParamsNoWaitAfter, frameClickParamsPosition, frameClickParamsStrict)
  , FrameClickResult
  , FrameDblclickParams (frameDblclickParamsStrict)
  , FrameDblclickResult
  , FrameDispatchEventParams (frameDispatchEventParamsStrict)
  , FrameDispatchEventResult
  , FrameDragAndDropParams (frameDragAndDropParamsStrict)
  , FrameDragAndDropResult
  , FrameDropParams (frameDropParamsData, frameDropParamsPayloads, frameDropParamsStrict)
  , FrameDropParamsData
  , FrameDropParamsPayloads (frameDropParamsPayloadsMimeType)
  , FrameDropResult
  , FrameFillParams (frameFillParamsStrict)
  , FrameFillResult
  , FrameFocusParams (frameFocusParamsStrict)
  , FrameFocusResult
  , FrameHoverParams (frameHoverParamsStrict)
  , FrameHoverResult
  , FrameHighlightResult
  , FrameHideHighlightResult
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
  , mkFrameBlurParams
  , mkFrameCheckParams
  , mkFrameClickParams
  , mkFrameDblclickParams
  , mkFrameDispatchEventParams
  , mkFrameDragAndDropParams
  , mkFrameDropParams
  , mkFrameDropParamsData
  , mkFrameDropParamsPayloads
  , mkFrameFillParams
  , mkFrameFocusParams
  , mkFrameHoverParams
  , mkFrameHighlightParams
  , mkFrameHideHighlightParams
  , mkFramePressParams
  , mkFrameSelectOptionParams
  , mkFrameSelectOptionParamsOptions
  , mkFrameSetInputFilesParams
  , mkFrameSetInputFilesParamsPayloads
  , mkFrameTapParams
  , mkFrameTypeParams
  , mkFrameUncheckParams
  )
import Playwright.Protocol qualified as Protocol
  ( Point
  , mkPoint
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
  clickFrameWith frame selector timeout defaultClickOptions

clickFrameWith :: Frame -> Selector -> Timeout -> ClickOptions -> IO ()
clickFrameWith frame selector timeout opts =
  frameSend_ @FrameClickResult
    frame
    "click"
    (mkFrameClickParams selector timeout)
      { frameClickParamsStrict = Just True
      , frameClickParamsForce = clickOptionsForce opts
      , frameClickParamsNoWaitAfter = clickOptionsNoWaitAfter opts
      , frameClickParamsPosition = fmap toProtocolPoint (clickOptionsPosition opts)
      }

toProtocolPoint :: Point -> Protocol.Point
toProtocolPoint point =
  Protocol.mkPoint (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point))

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

blurFrame :: Frame -> Selector -> Timeout -> IO ()
blurFrame frame selector timeout =
  frameSend_ @FrameBlurResult
    frame
    "blur"
    (mkFrameBlurParams selector timeout)
      { frameBlurParamsStrict = Just True
      }

hoverFrame :: Frame -> Selector -> Timeout -> IO ()
hoverFrame frame selector timeout =
  frameSend_ @FrameHoverResult
    frame
    "hover"
    (mkFrameHoverParams selector timeout)
      { frameHoverParamsStrict = Just True
      }

highlightFrame :: Frame -> Selector -> IO ()
highlightFrame frame selector =
  frameSend_ @FrameHighlightResult
    frame
    "highlight"
    (mkFrameHighlightParams selector)

hideHighlightFrame :: Frame -> Selector -> IO ()
hideHighlightFrame frame selector =
  frameSend_ @FrameHideHighlightResult
    frame
    "hideHighlight"
    (mkFrameHideHighlightParams selector)

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

dropFrame :: Frame -> Selector -> DropPayload -> Timeout -> IO ()
dropFrame frame selector payload timeout =
  if null (dropPayloadFiles payload) && null (dropPayloadData payload)
    then
      throwIO (DecodeFailure "At least one of \"files\" or \"data\" must be provided")
    else
      frameSend_ @FrameDropResult
        frame
        "drop"
        (mkFrameDropParams selector timeout)
          { frameDropParamsStrict = Just True
          , frameDropParamsPayloads =
              if null (dropPayloadFiles payload)
                then Nothing
                else Just (fmap toDropFilePayload (dropPayloadFiles payload))
          , frameDropParamsData =
              if null (dropPayloadData payload)
                then Nothing
                else Just (fmap toDropDataPayload (dropPayloadData payload))
          }

toDropFilePayload :: DropFile -> FrameDropParamsPayloads
toDropFilePayload file =
  ( mkFrameDropParamsPayloads
      (protocolBinaryFromByteString (dropFileBytes file))
      (inputFileNameToText (dropFileName file))
  )
    { frameDropParamsPayloadsMimeType = fmap mimeTypeToText (dropFileMimeType file)
    }

toDropDataPayload :: DropData -> FrameDropParamsData
toDropDataPayload item =
  mkFrameDropParamsData (mimeTypeToText (dropDataMimeType item)) (dropDataValue item)

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

module Playwright.Frame.Action
  ( blurFrame
  , checkFrame
  , clickFrame
  , clickFrameStrict
  , clickFrameWith
  , dblclickFrame
  , dblclickFrameWith
  , dispatchEventFrame
  , dispatchEventFrameWith
  , fillFrame
  , focusFrame
  , hoverFrame
  , hoverFrameWith
  , highlightFrame
  , hideHighlightFrame
  , pressFrame
  , selectOptionFrame
  , selectOptionFrameWith
  , selectOptionElementsFrameWith
  , tapFrame
  , tapFrameWith
  , typeTextFrame
  , uncheckFrame
  , dragAndDropFrame
  , dragAndDropFrameWith
  , dropFrame
  , setInputFilesFrame
  ) where

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
  ( throwIO
  )
import Data.Text
  ( Text
  )
import Playwright.CheckOptions
  ( CheckOptions (checkOptionsForce, checkOptionsPosition, checkOptionsTimeout, checkOptionsTrial)
  )
import Playwright.ClickOptions
  ( ClickOptions (clickOptionsButton, clickOptionsClickCount, clickOptionsForce, clickOptionsModifiers, clickOptionsNoWaitAfter, clickOptionsPosition, clickOptionsScroll, clickOptionsSteps, clickOptionsTrial)
  , defaultClickOptions
  )
import Playwright.CssPixel
  ( cssPixelToScientific
  )
import Playwright.DomEventType
  ( DomEventType (domEventTypeToText)
  )
import Playwright.DragAndDropOptions
  ( DragAndDropOptions (dragAndDropOptionsForce, dragAndDropOptionsSourcePosition, dragAndDropOptionsSteps, dragAndDropOptionsTargetPosition, dragAndDropOptionsTrial)
  , defaultDragAndDropOptions
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
  , frameSend_
  , frameSendTimed_
  , frameSendWithTimeout
  )
import Playwright.InputFile
  ( InputFile (inputFileBytes, inputFileName)
  )
import Playwright.InputFileName
  ( inputFileNameToText
  )
import Playwright.Json.ChannelRef
  ( ChannelRef
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryFromByteString
  )
import Playwright.Key
  ( Key (keyToText)
  )
import Playwright.KeyboardModifier
  ( KeyboardModifier (KeyboardModifierAlt, KeyboardModifierControl, KeyboardModifierControlOrMeta, KeyboardModifierMeta, KeyboardModifierShift)
  )
import Playwright.MimeType
  ( mimeTypeToText
  )
import Playwright.MouseButton
  ( MouseButton (MouseButtonLeft, MouseButtonMiddle, MouseButtonRight)
  )
import Playwright.Point
  ( Point
  , pointX
  , pointY
  )
import Playwright.Protocol
  ( FrameBlurParams (frameBlurParamsStrict)
  , FrameBlurResult
  , FrameCheckParams (frameCheckParamsForce, frameCheckParamsPosition, frameCheckParamsStrict, frameCheckParamsTrial)
  , FrameCheckResult
  , FrameClickParams (frameClickParamsButton, frameClickParamsClickCount, frameClickParamsForce, frameClickParamsModifiers, frameClickParamsNoWaitAfter, frameClickParamsPosition, frameClickParamsScroll, frameClickParamsSteps, frameClickParamsStrict, frameClickParamsTrial)
  , FrameClickParamsButton (FrameClickParamsButtonLeft, FrameClickParamsButtonMiddle, FrameClickParamsButtonRight)
  , FrameClickParamsModifiers (FrameClickParamsModifiersAlt, FrameClickParamsModifiersControl, FrameClickParamsModifiersControlOrMeta, FrameClickParamsModifiersMeta, FrameClickParamsModifiersShift)
  , FrameClickParamsScroll (FrameClickParamsScrollAuto, FrameClickParamsScrollNone)
  , FrameClickResult
  , FrameDblclickParams (frameDblclickParamsForce, frameDblclickParamsModifiers, frameDblclickParamsPosition, frameDblclickParamsScroll, frameDblclickParamsStrict, frameDblclickParamsTrial)
  , FrameDblclickParamsModifiers (FrameDblclickParamsModifiersAlt, FrameDblclickParamsModifiersControl, FrameDblclickParamsModifiersControlOrMeta, FrameDblclickParamsModifiersMeta, FrameDblclickParamsModifiersShift)
  , FrameDblclickParamsScroll (FrameDblclickParamsScrollAuto, FrameDblclickParamsScrollNone)
  , FrameDblclickResult
  , FrameDispatchEventParams (frameDispatchEventParamsStrict)
  , FrameDispatchEventResult
  , FrameDragAndDropParams (frameDragAndDropParamsForce, frameDragAndDropParamsSourcePosition, frameDragAndDropParamsSteps, frameDragAndDropParamsStrict, frameDragAndDropParamsTargetPosition, frameDragAndDropParamsTrial)
  , FrameDragAndDropResult
  , FrameDropParams (frameDropParamsData, frameDropParamsPayloads, frameDropParamsStrict)
  , FrameDropParamsData
  , FrameDropParamsPayloads (frameDropParamsPayloadsMimeType)
  , FrameDropResult
  , FrameFillParams (frameFillParamsStrict)
  , FrameFillResult
  , FrameFocusParams (frameFocusParamsStrict)
  , FrameFocusResult
  , FrameHoverParams (frameHoverParamsForce, frameHoverParamsModifiers, frameHoverParamsPosition, frameHoverParamsScroll, frameHoverParamsStrict, frameHoverParamsTrial)
  , FrameHoverParamsModifiers (FrameHoverParamsModifiersAlt, FrameHoverParamsModifiersControl, FrameHoverParamsModifiersControlOrMeta, FrameHoverParamsModifiersMeta, FrameHoverParamsModifiersShift)
  , FrameHoverParamsScroll (FrameHoverParamsScrollAuto, FrameHoverParamsScrollNone)
  , FrameHoverResult
  , FrameHighlightResult
  , FrameHideHighlightResult
  , FramePressParams (framePressParamsStrict)
  , FramePressResult
  , FrameSelectOptionParams (frameSelectOptionParamsElements, frameSelectOptionParamsForce, frameSelectOptionParamsOptions, frameSelectOptionParamsStrict)
  , FrameSelectOptionParamsOptions (frameSelectOptionParamsOptionsIndex, frameSelectOptionParamsOptionsLabel, frameSelectOptionParamsOptionsValue, frameSelectOptionParamsOptionsValueOrLabel)
  , FrameSelectOptionResult (frameSelectOptionResultValues)
  , FrameSetInputFilesParams (frameSetInputFilesParamsPayloads, frameSetInputFilesParamsStrict)
  , FrameSetInputFilesParamsPayloads
  , FrameSetInputFilesResult
  , FrameTapParams (frameTapParamsForce, frameTapParamsModifiers, frameTapParamsPosition, frameTapParamsStrict, frameTapParamsTrial)
  , FrameTapParamsModifiers (FrameTapParamsModifiersAlt, FrameTapParamsModifiersControl, FrameTapParamsModifiersControlOrMeta, FrameTapParamsModifiersMeta, FrameTapParamsModifiersShift)
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
  ( SerializedArgument
  , undefinedArgument
  )
import Playwright.SelectOption
  ( SelectAttributes (selectAttributesIndex, selectAttributesLabel, selectAttributesValue)
  , SelectOption (SelectIndex, SelectLabel, SelectMatching, SelectValue, SelectValueOrLabel)
  )
import Playwright.SelectOptionOptions
  ( SelectOptionOptions (selectOptionOptionsForce)
  , defaultSelectOptionOptions
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.DblclickOptions
  ( DblclickOptions (dblclickOptionsForce, dblclickOptionsModifiers, dblclickOptionsPosition, dblclickOptionsScroll, dblclickOptionsTrial)
  , defaultDblclickOptions
  )
import Playwright.HoverOptions
  ( HoverOptions (hoverOptionsForce, hoverOptionsModifiers, hoverOptionsPosition, hoverOptionsScroll, hoverOptionsTrial)
  , defaultHoverOptions
  )
import Playwright.ScrollOption
  ( ScrollOption (ScrollAuto, ScrollNone)
  )
import Playwright.TapOptions
  ( TapOptions (tapOptionsForce, tapOptionsModifiers, tapOptionsPosition, tapOptionsTrial)
  , defaultTapOptions
  )
import Playwright.Timeout
  ( Timeout
  )

clickFrame :: Frame -> Selector -> Timeout -> Playwright ()
clickFrame frame selector timeout =
  clickFrameWith frame selector timeout defaultClickOptions

clickFrameStrict :: Frame -> Selector -> Timeout -> Maybe Bool -> Playwright ()
clickFrameStrict frame selector timeout strict =
  frameSendTimed_ @FrameClickResult
    frame
    "click"
    timeout
    (mkFrameClickParams selector)
      { frameClickParamsStrict = strict
      }

clickFrameWith :: Frame -> Selector -> Timeout -> ClickOptions -> Playwright ()
clickFrameWith frame selector timeout opts =
  frameSendTimed_ @FrameClickResult
    frame
    "click"
    timeout
    (mkFrameClickParams selector)
      { frameClickParamsStrict = Just True
      , frameClickParamsButton = fmap toFrameClickButton (clickOptionsButton opts)
      , frameClickParamsClickCount = clickOptionsClickCount opts
      , frameClickParamsForce = clickOptionsForce opts
      , frameClickParamsModifiers = fmap (fmap toFrameClickModifier) (clickOptionsModifiers opts)
      , frameClickParamsNoWaitAfter = clickOptionsNoWaitAfter opts
      , frameClickParamsPosition = fmap toProtocolPoint (clickOptionsPosition opts)
      , frameClickParamsScroll = fmap toFrameClickScroll (clickOptionsScroll opts)
      , frameClickParamsSteps = clickOptionsSteps opts
      , frameClickParamsTrial = clickOptionsTrial opts
      }

toFrameClickButton :: MouseButton -> FrameClickParamsButton
toFrameClickButton button =
  case button of
    MouseButtonLeft ->
      FrameClickParamsButtonLeft
    MouseButtonRight ->
      FrameClickParamsButtonRight
    MouseButtonMiddle ->
      FrameClickParamsButtonMiddle

toFrameClickScroll :: ScrollOption -> FrameClickParamsScroll
toFrameClickScroll scroll =
  case scroll of
    ScrollAuto ->
      FrameClickParamsScrollAuto
    ScrollNone ->
      FrameClickParamsScrollNone

toFrameClickModifier :: KeyboardModifier -> FrameClickParamsModifiers
toFrameClickModifier modifier =
  case modifier of
    KeyboardModifierAlt ->
      FrameClickParamsModifiersAlt
    KeyboardModifierControl ->
      FrameClickParamsModifiersControl
    KeyboardModifierControlOrMeta ->
      FrameClickParamsModifiersControlOrMeta
    KeyboardModifierMeta ->
      FrameClickParamsModifiersMeta
    KeyboardModifierShift ->
      FrameClickParamsModifiersShift

toProtocolPoint :: Point -> Protocol.Point
toProtocolPoint point =
  Protocol.mkPoint (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point))

dblclickFrame :: Frame -> Selector -> Timeout -> Playwright ()
dblclickFrame frame selector timeout =
  dblclickFrameWith frame selector timeout defaultDblclickOptions

dblclickFrameWith :: Frame -> Selector -> Timeout -> DblclickOptions -> Playwright ()
dblclickFrameWith frame selector timeout opts =
  frameSendTimed_ @FrameDblclickResult
    frame
    "dblclick"
    timeout
    (mkFrameDblclickParams selector)
      { frameDblclickParamsStrict = Just True
      , frameDblclickParamsForce = dblclickOptionsForce opts
      , frameDblclickParamsModifiers = fmap (fmap toFrameDblclickModifier) (dblclickOptionsModifiers opts)
      , frameDblclickParamsPosition = fmap toProtocolPoint (dblclickOptionsPosition opts)
      , frameDblclickParamsScroll = fmap toFrameDblclickScroll (dblclickOptionsScroll opts)
      , frameDblclickParamsTrial = dblclickOptionsTrial opts
      }

toFrameDblclickScroll :: ScrollOption -> FrameDblclickParamsScroll
toFrameDblclickScroll scroll =
  case scroll of
    ScrollAuto ->
      FrameDblclickParamsScrollAuto
    ScrollNone ->
      FrameDblclickParamsScrollNone

toFrameDblclickModifier :: KeyboardModifier -> FrameDblclickParamsModifiers
toFrameDblclickModifier modifier =
  case modifier of
    KeyboardModifierAlt ->
      FrameDblclickParamsModifiersAlt
    KeyboardModifierControl ->
      FrameDblclickParamsModifiersControl
    KeyboardModifierControlOrMeta ->
      FrameDblclickParamsModifiersControlOrMeta
    KeyboardModifierMeta ->
      FrameDblclickParamsModifiersMeta
    KeyboardModifierShift ->
      FrameDblclickParamsModifiersShift

dispatchEventFrame :: Frame -> Selector -> DomEventType -> Timeout -> Playwright ()
dispatchEventFrame frame selector eventType timeout =
  dispatchEventFrameWith frame selector eventType undefinedArgument timeout

dispatchEventFrameWith :: Frame -> Selector -> DomEventType -> SerializedArgument -> Timeout -> Playwright ()
dispatchEventFrameWith frame selector eventType eventInit timeout =
  frameSendTimed_ @FrameDispatchEventResult
    frame
    "dispatchEvent"
    timeout
    (mkFrameDispatchEventParams eventInit selector (domEventTypeToText eventType))
      { frameDispatchEventParamsStrict = Just True
      }

fillFrame :: Frame -> Selector -> Text -> Timeout -> Playwright ()
fillFrame frame selector value timeout =
  frameSendTimed_ @FrameFillResult
    frame
    "fill"
    timeout
    (mkFrameFillParams selector value)
      { frameFillParamsStrict = Just True
      }

checkFrame :: Frame -> Selector -> CheckOptions -> Playwright ()
checkFrame frame selector opts =
  frameSendTimed_ @FrameCheckResult
    frame
    "check"
    (checkOptionsTimeout opts)
    (mkFrameCheckParams selector)
      { frameCheckParamsStrict = Just True
      , frameCheckParamsForce = checkOptionsForce opts
      , frameCheckParamsPosition = fmap toProtocolPoint (checkOptionsPosition opts)
      , frameCheckParamsTrial = checkOptionsTrial opts
      }

uncheckFrame :: Frame -> Selector -> CheckOptions -> Playwright ()
uncheckFrame frame selector opts =
  frameSendTimed_ @FrameUncheckResult
    frame
    "uncheck"
    (checkOptionsTimeout opts)
    (mkFrameUncheckParams selector)
      { frameUncheckParamsStrict = Just True
      , frameUncheckParamsForce = checkOptionsForce opts
      , frameUncheckParamsTrial = checkOptionsTrial opts
      }

focusFrame :: Frame -> Selector -> Timeout -> Playwright ()
focusFrame frame selector timeout =
  frameSendTimed_ @FrameFocusResult
    frame
    "focus"
    timeout
    (mkFrameFocusParams selector)
      { frameFocusParamsStrict = Just True
      }

blurFrame :: Frame -> Selector -> Timeout -> Playwright ()
blurFrame frame selector timeout =
  frameSendTimed_ @FrameBlurResult
    frame
    "blur"
    timeout
    (mkFrameBlurParams selector)
      { frameBlurParamsStrict = Just True
      }

hoverFrame :: Frame -> Selector -> Timeout -> Playwright ()
hoverFrame frame selector timeout =
  hoverFrameWith frame selector timeout defaultHoverOptions

hoverFrameWith :: Frame -> Selector -> Timeout -> HoverOptions -> Playwright ()
hoverFrameWith frame selector timeout opts =
  frameSendTimed_ @FrameHoverResult
    frame
    "hover"
    timeout
    (mkFrameHoverParams selector)
      { frameHoverParamsStrict = Just True
      , frameHoverParamsForce = hoverOptionsForce opts
      , frameHoverParamsModifiers = fmap (fmap toFrameHoverModifier) (hoverOptionsModifiers opts)
      , frameHoverParamsPosition = fmap toProtocolPoint (hoverOptionsPosition opts)
      , frameHoverParamsScroll = fmap toFrameHoverScroll (hoverOptionsScroll opts)
      , frameHoverParamsTrial = hoverOptionsTrial opts
      }

toFrameHoverScroll :: ScrollOption -> FrameHoverParamsScroll
toFrameHoverScroll scroll =
  case scroll of
    ScrollAuto ->
      FrameHoverParamsScrollAuto
    ScrollNone ->
      FrameHoverParamsScrollNone

toFrameHoverModifier :: KeyboardModifier -> FrameHoverParamsModifiers
toFrameHoverModifier modifier =
  case modifier of
    KeyboardModifierAlt ->
      FrameHoverParamsModifiersAlt
    KeyboardModifierControl ->
      FrameHoverParamsModifiersControl
    KeyboardModifierControlOrMeta ->
      FrameHoverParamsModifiersControlOrMeta
    KeyboardModifierMeta ->
      FrameHoverParamsModifiersMeta
    KeyboardModifierShift ->
      FrameHoverParamsModifiersShift

highlightFrame :: Frame -> Selector -> Playwright ()
highlightFrame frame selector =
  frameSend_ @FrameHighlightResult
    frame
    "highlight"
    (mkFrameHighlightParams selector)

hideHighlightFrame :: Frame -> Selector -> Playwright ()
hideHighlightFrame frame selector =
  frameSend_ @FrameHideHighlightResult
    frame
    "hideHighlight"
    (mkFrameHideHighlightParams selector)

tapFrame :: Frame -> Selector -> Timeout -> Playwright ()
tapFrame frame selector timeout =
  tapFrameWith frame selector timeout defaultTapOptions

tapFrameWith :: Frame -> Selector -> Timeout -> TapOptions -> Playwright ()
tapFrameWith frame selector timeout opts =
  frameSendTimed_ @FrameTapResult
    frame
    "tap"
    timeout
    (mkFrameTapParams selector)
      { frameTapParamsStrict = Just True
      , frameTapParamsForce = tapOptionsForce opts
      , frameTapParamsModifiers = fmap (fmap toFrameTapModifier) (tapOptionsModifiers opts)
      , frameTapParamsPosition = fmap toProtocolPoint (tapOptionsPosition opts)
      , frameTapParamsTrial = tapOptionsTrial opts
      }

toFrameTapModifier :: KeyboardModifier -> FrameTapParamsModifiers
toFrameTapModifier modifier =
  case modifier of
    KeyboardModifierAlt ->
      FrameTapParamsModifiersAlt
    KeyboardModifierControl ->
      FrameTapParamsModifiersControl
    KeyboardModifierControlOrMeta ->
      FrameTapParamsModifiersControlOrMeta
    KeyboardModifierMeta ->
      FrameTapParamsModifiersMeta
    KeyboardModifierShift ->
      FrameTapParamsModifiersShift

pressFrame :: Frame -> Selector -> Key -> Timeout -> Playwright ()
pressFrame frame selector key timeout =
  frameSendTimed_ @FramePressResult
    frame
    "press"
    timeout
    (mkFramePressParams (keyToText key) selector)
      { framePressParamsStrict = Just True
      }

typeTextFrame :: Frame -> Selector -> Text -> Timeout -> Playwright ()
typeTextFrame frame selector text timeout =
  frameSendTimed_ @FrameTypeResult
    frame
    "type"
    timeout
    (mkFrameTypeParams selector text)
      { frameTypeParamsStrict = Just True
      }

dragAndDropFrame :: Frame -> Selector -> Selector -> Timeout -> Playwright ()
dragAndDropFrame frame source target timeout =
  dragAndDropFrameWith frame source target timeout defaultDragAndDropOptions

dragAndDropFrameWith :: Frame -> Selector -> Selector -> Timeout -> DragAndDropOptions -> Playwright ()
dragAndDropFrameWith frame source target timeout opts =
  frameSendTimed_ @FrameDragAndDropResult
    frame
    "dragAndDrop"
    timeout
    (mkFrameDragAndDropParams source target)
      { frameDragAndDropParamsStrict = Just True
      , frameDragAndDropParamsForce = dragAndDropOptionsForce opts
      , frameDragAndDropParamsSourcePosition = fmap toProtocolPoint (dragAndDropOptionsSourcePosition opts)
      , frameDragAndDropParamsSteps = dragAndDropOptionsSteps opts
      , frameDragAndDropParamsTargetPosition = fmap toProtocolPoint (dragAndDropOptionsTargetPosition opts)
      , frameDragAndDropParamsTrial = dragAndDropOptionsTrial opts
      }

dropFrame :: Frame -> Selector -> DropPayload -> Timeout -> Playwright ()
dropFrame frame selector payload timeout =
  if null (dropPayloadFiles payload) && null (dropPayloadData payload)
    then
      throwIO (DecodeFailure "At least one of \"files\" or \"data\" must be provided")
    else
      frameSendTimed_ @FrameDropResult
        frame
        "drop"
        timeout
        (mkFrameDropParams selector)
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

setInputFilesFrame :: Frame -> Selector -> [InputFile] -> Timeout -> Playwright ()
setInputFilesFrame frame selector files timeout =
  frameSendTimed_ @FrameSetInputFilesResult
    frame
    "setInputFiles"
    timeout
    (mkFrameSetInputFilesParams selector)
      { frameSetInputFilesParamsStrict = Just True
      , frameSetInputFilesParamsPayloads = Just (fmap toPayload files)
      }

toPayload :: InputFile -> FrameSetInputFilesParamsPayloads
toPayload file =
  mkFrameSetInputFilesParamsPayloads
    (protocolBinaryFromByteString (inputFileBytes file))
    (inputFileNameToText (inputFileName file))

selectOptionFrame :: Frame -> Selector -> [SelectOption] -> Timeout -> Playwright [Text]
selectOptionFrame frame selector options timeout =
  selectOptionFrameWith frame selector options timeout defaultSelectOptionOptions

selectOptionFrameWith :: Frame -> Selector -> [SelectOption] -> Timeout -> SelectOptionOptions -> Playwright [Text]
selectOptionFrameWith frame selector options timeout opts = do
  result <-
    frameSendWithTimeout
      frame
      "selectOption"
      timeout
      ( (mkFrameSelectOptionParams selector)
          { frameSelectOptionParamsStrict = Just True
          , frameSelectOptionParamsForce = selectOptionOptionsForce opts
          , frameSelectOptionParamsOptions = Just (fmap toProtocolOption options)
          }
      )
  pure (frameSelectOptionResultValues result)

-- Element-handle form: the handles travel in the `elements` list, and `options` stays unset.
selectOptionElementsFrameWith :: Frame -> Selector -> [ChannelRef] -> Timeout -> SelectOptionOptions -> Playwright [Text]
selectOptionElementsFrameWith frame selector elements timeout opts = do
  result <-
    frameSendWithTimeout
      frame
      "selectOption"
      timeout
      ( (mkFrameSelectOptionParams selector)
          { frameSelectOptionParamsStrict = Just True
          , frameSelectOptionParamsForce = selectOptionOptionsForce opts
          , frameSelectOptionParamsElements = Just elements
          }
      )
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
    SelectMatching attributes ->
      mkFrameSelectOptionParamsOptions
        { frameSelectOptionParamsOptionsValue = selectAttributesValue attributes
        , frameSelectOptionParamsOptionsLabel = selectAttributesLabel attributes
        , frameSelectOptionParamsOptionsIndex = selectAttributesIndex attributes
        }

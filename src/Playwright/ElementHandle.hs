module Playwright.ElementHandle
  ( ElementHandle (..)
  , boundingBox
  , clickHandle
  , clickHandleWith
  , dblclickHandle
  , dblclickHandleWith
  , dispatchEventHandle
  , dispatchEventHandleWith
  , tapHandle
  , tapHandleWith
  , elementArgument
  , contentFrameHandle
  , ownerFrameHandle
  , elementHandleFromRef
  , getAttributeHandle
  , innerHtmlHandle
  , innerTextHandle
  , inputValueHandle
  , isCheckedHandle
  , isEditableHandle
  , isDisabledHandle
  , isEnabledHandle
  , isHiddenHandle
  , isVisibleHandle
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , screenshotHandle
  , screenshotHandleWith
  , selectText
  , selectTextWith
  , setInputFilesHandle
  , textContentHandle
  , typeHandle
  , waitForElementState
  , waitForElementStateWith
  ) where

import Playwright.Monad
  ( Playwright
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import UnliftIO.Exception
  ( throwIO
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.ByteString
  ( ByteString
  )
import Data.Text
  ( Text
  )
import Playwright.BoundingBox
  ( BoundingBox (BoundingBox, boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  )
import Playwright.Channel
  ( Channel (Channel, channelGuid)
  , ElementHandleTag
  , send
  , sendWithTimeout
  )
import Playwright.ClickOptions
  ( ClickOptions (clickOptionsButton, clickOptionsClickCount, clickOptionsForce, clickOptionsModifiers, clickOptionsNoWaitAfter, clickOptionsPosition, clickOptionsSteps, clickOptionsTimeout, clickOptionsTrial)
  , defaultClickOptions
  )
import Playwright.DblclickOptions
  ( DblclickOptions (dblclickOptionsForce, dblclickOptionsModifiers, dblclickOptionsPosition, dblclickOptionsTimeout, dblclickOptionsTrial)
  , defaultDblclickOptions
  )
import Playwright.CssPixel
  ( CssPixel (CssPixel)
  , cssPixelToScientific
  )
import Playwright.DomEventType
  ( DomEventType (domEventTypeToText)
  )
import Playwright.ElementState
  ( ElementState (ElementDisabled, ElementEditable, ElementEnabled, ElementHidden, ElementStable, ElementVisible)
  )
import Playwright.Frame
  ( Frame
  , frameFromRef
  )
import Playwright.InputFile
  ( InputFile (inputFileBytes, inputFileName)
  )
import Playwright.InputFileName
  ( inputFileNameToText
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.Json.ProtocolBinary
  ( protocolBinaryFromByteString
  , protocolBinaryToByteString
  )
import Playwright.KeyboardModifier
  ( KeyboardModifier (KeyboardModifierAlt, KeyboardModifierControl, KeyboardModifierControlOrMeta, KeyboardModifierMeta, KeyboardModifierShift)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.MouseButton
  ( MouseButton (MouseButtonLeft, MouseButtonMiddle, MouseButtonRight)
  )
import Playwright.Point
  ( Point
  , pointX
  , pointY
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.ScreenshotOptions
  ( ScreenshotAnimations (ScreenshotAnimationsAllow, ScreenshotAnimationsDisabled)
  , ScreenshotCaret (ScreenshotCaretHide, ScreenshotCaretInitial)
  , ScreenshotOptions (screenshotOptionsAnimations, screenshotOptionsCaret, screenshotOptionsMaskColor, screenshotOptionsOmitBackground, screenshotOptionsPath, screenshotOptionsQuality, screenshotOptionsScale, screenshotOptionsStyle, screenshotOptionsTimeout)
  , ScreenshotScale (ScreenshotScaleCss, ScreenshotScaleDevice)
  , ScreenshotType (ScreenshotJpeg, ScreenshotPng, ScreenshotWebp)
  , resolveScreenshotType
  , writeScreenshotFile
  )
import Playwright.Protocol
  ( ElementHandleBoundingBoxResult (elementHandleBoundingBoxResultValue)
  , ElementHandleClickParams (elementHandleClickParamsButton, elementHandleClickParamsClickCount, elementHandleClickParamsForce, elementHandleClickParamsModifiers, elementHandleClickParamsNoWaitAfter, elementHandleClickParamsPosition, elementHandleClickParamsSteps, elementHandleClickParamsTrial)
  , ElementHandleClickParamsButton (ElementHandleClickParamsButtonLeft, ElementHandleClickParamsButtonMiddle, ElementHandleClickParamsButtonRight)
  , ElementHandleClickParamsModifiers (ElementHandleClickParamsModifiersAlt, ElementHandleClickParamsModifiersControl, ElementHandleClickParamsModifiersControlOrMeta, ElementHandleClickParamsModifiersMeta, ElementHandleClickParamsModifiersShift)
  , ElementHandleClickResult
  , ElementHandleDblclickParams (elementHandleDblclickParamsForce, elementHandleDblclickParamsModifiers, elementHandleDblclickParamsPosition, elementHandleDblclickParamsTrial)
  , ElementHandleDblclickParamsModifiers (ElementHandleDblclickParamsModifiersAlt, ElementHandleDblclickParamsModifiersControl, ElementHandleDblclickParamsModifiersControlOrMeta, ElementHandleDblclickParamsModifiersMeta, ElementHandleDblclickParamsModifiersShift)
  , ElementHandleDblclickResult
  , ElementHandleDispatchEventResult
  , ElementHandleContentFrameResult (elementHandleContentFrameResultFrame)
  , ElementHandleGetAttributeResult (elementHandleGetAttributeResultValue)
  , ElementHandleInnerHTMLResult (elementHandleInnerHTMLResultValue)
  , ElementHandleInnerTextResult (elementHandleInnerTextResultValue)
  , ElementHandleInputValueResult (elementHandleInputValueResultValue)
  , ElementHandleIsCheckedResult (elementHandleIsCheckedResultValue)
  , ElementHandleIsEditableResult (elementHandleIsEditableResultValue)
  , ElementHandleIsEnabledResult (elementHandleIsEnabledResultValue)
  , ElementHandleIsHiddenResult (elementHandleIsHiddenResultValue)
  , ElementHandleIsVisibleResult (elementHandleIsVisibleResultValue)
  , ElementHandleOwnerFrameResult (elementHandleOwnerFrameResultFrame)
  , ElementHandleScreenshotParams (elementHandleScreenshotParamsAnimations, elementHandleScreenshotParamsCaret, elementHandleScreenshotParamsMask, elementHandleScreenshotParamsMaskColor, elementHandleScreenshotParamsOmitBackground, elementHandleScreenshotParamsQuality, elementHandleScreenshotParamsScale, elementHandleScreenshotParamsStyle, elementHandleScreenshotParamsType)
  , ElementHandleScreenshotParamsAnimations (ElementHandleScreenshotParamsAnimationsAllow, ElementHandleScreenshotParamsAnimationsDisabled)
  , ElementHandleScreenshotParamsCaret (ElementHandleScreenshotParamsCaretHide, ElementHandleScreenshotParamsCaretInitial)
  , ElementHandleScreenshotParamsMask
  , ElementHandleScreenshotParamsScale (ElementHandleScreenshotParamsScaleCss, ElementHandleScreenshotParamsScaleDevice)
  , ElementHandleScreenshotParamsType (ElementHandleScreenshotParamsTypeJpeg, ElementHandleScreenshotParamsTypePng, ElementHandleScreenshotParamsTypeWebp)
  , ElementHandleScreenshotResult (elementHandleScreenshotResultBinary)
  , ElementHandleScrollIntoViewIfNeededResult
  , ElementHandleSelectTextResult
  , ElementHandleSetInputFilesParams (elementHandleSetInputFilesParamsPayloads)
  , ElementHandleSetInputFilesParamsPayloads
  , ElementHandleSetInputFilesResult
  , ElementHandleTapParams (elementHandleTapParamsForce, elementHandleTapParamsModifiers, elementHandleTapParamsPosition, elementHandleTapParamsTrial)
  , ElementHandleTapParamsModifiers (ElementHandleTapParamsModifiersAlt, ElementHandleTapParamsModifiersControl, ElementHandleTapParamsModifiersControlOrMeta, ElementHandleTapParamsModifiersMeta, ElementHandleTapParamsModifiersShift)
  , ElementHandleTapResult
  , ElementHandleTextContentResult (elementHandleTextContentResultValue)
  , ElementHandleTypeResult
  , ElementHandleWaitForElementStateParamsState (ElementHandleWaitForElementStateParamsStateDisabled, ElementHandleWaitForElementStateParamsStateEditable, ElementHandleWaitForElementStateParamsStateEnabled, ElementHandleWaitForElementStateParamsStateHidden, ElementHandleWaitForElementStateParamsStateStable, ElementHandleWaitForElementStateParamsStateVisible)
  , ElementHandleWaitForElementStateResult
  , Rect (rectHeight, rectWidth, rectX, rectY)
  , mkElementHandleBoundingBoxParams
  , mkElementHandleClickParams
  , mkElementHandleDblclickParams
  , mkElementHandleDispatchEventParams
  , mkElementHandleContentFrameParams
  , mkElementHandleGetAttributeParams
  , mkElementHandleInnerHTMLParams
  , mkElementHandleInnerTextParams
  , mkElementHandleInputValueParams
  , mkElementHandleIsCheckedParams
  , mkElementHandleIsEditableParams
  , mkElementHandleIsEnabledParams
  , mkElementHandleIsHiddenParams
  , mkElementHandleIsVisibleParams
  , mkElementHandleOwnerFrameParams
  , mkElementHandleScreenshotParams
  , mkElementHandleScrollIntoViewIfNeededParams
  , mkElementHandleSelectTextParams
  , mkElementHandleSetInputFilesParams
  , mkElementHandleSetInputFilesParamsPayloads
  , mkElementHandleTapParams
  , mkElementHandleTextContentParams
  , mkElementHandleTypeParams
  , mkElementHandleWaitForElementStateParams
  )
import Playwright.Protocol qualified as Protocol
  ( Point
  , mkPoint
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , channelArgument
  , undefinedArgument
  )
import Playwright.TapOptions
  ( TapOptions (tapOptionsForce, tapOptionsModifiers, tapOptionsPosition, tapOptionsTimeout, tapOptionsTrial)
  , defaultTapOptions
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )

newtype ElementHandle = ElementHandle
  { elementHandleChannel :: Channel ElementHandleTag
  }

elementHandleFromRef :: ChannelRef -> ElementHandle
elementHandleFromRef ref =
  ElementHandle (Channel (channelRefGuid ref))

elementArgument :: ElementHandle -> SerializedArgument
elementArgument handle =
  channelArgument (ChannelRef (channelGuid (elementHandleChannel handle)))

handleSend :: (ToJSON params, FromJSON result) => ElementHandle -> Text -> params -> Playwright result
handleSend handle method =
  send (elementHandleChannel handle) (MethodName method)

-- | Sends a handle call whose timeout travels in the message metadata.
handleSendWithTimeout :: (ToJSON params, FromJSON result) => ElementHandle -> Text -> Timeout -> params -> Playwright result
handleSendWithTimeout handle method timeout params =
  sendWithTimeout (elementHandleChannel handle) (MethodName method) timeout params

clickHandle :: ElementHandle -> Playwright ()
clickHandle handle =
  clickHandleWith handle defaultClickOptions

clickHandleWith :: ElementHandle -> ClickOptions -> Playwright ()
clickHandleWith handle opts = do
  timeout <-
    case clickOptionsTimeout opts of
      Just value ->
        pure value
      Nothing ->
        pure defaultTimeout
  _ :: ElementHandleClickResult <-
    handleSendWithTimeout
      handle
      "click"
      timeout
      (mkElementHandleClickParams)
        { elementHandleClickParamsButton = fmap toHandleClickButton (clickOptionsButton opts)
        , elementHandleClickParamsClickCount = clickOptionsClickCount opts
        , elementHandleClickParamsForce = clickOptionsForce opts
        , elementHandleClickParamsModifiers = fmap (fmap toHandleClickModifier) (clickOptionsModifiers opts)
        , elementHandleClickParamsNoWaitAfter = clickOptionsNoWaitAfter opts
        , elementHandleClickParamsPosition = fmap toProtocolPoint (clickOptionsPosition opts)
        , elementHandleClickParamsSteps = clickOptionsSteps opts
        , elementHandleClickParamsTrial = clickOptionsTrial opts
        }
  pure ()

toHandleClickButton :: MouseButton -> ElementHandleClickParamsButton
toHandleClickButton button =
  case button of
    MouseButtonLeft ->
      ElementHandleClickParamsButtonLeft
    MouseButtonRight ->
      ElementHandleClickParamsButtonRight
    MouseButtonMiddle ->
      ElementHandleClickParamsButtonMiddle

toHandleClickModifier :: KeyboardModifier -> ElementHandleClickParamsModifiers
toHandleClickModifier modifier =
  case modifier of
    KeyboardModifierAlt ->
      ElementHandleClickParamsModifiersAlt
    KeyboardModifierControl ->
      ElementHandleClickParamsModifiersControl
    KeyboardModifierControlOrMeta ->
      ElementHandleClickParamsModifiersControlOrMeta
    KeyboardModifierMeta ->
      ElementHandleClickParamsModifiersMeta
    KeyboardModifierShift ->
      ElementHandleClickParamsModifiersShift

dblclickHandle :: ElementHandle -> Playwright ()
dblclickHandle handle =
  dblclickHandleWith handle defaultDblclickOptions

-- | Double-clicks through the handle. The protocol's handle dblclick takes no
-- button, steps or delay, so those fields of 'DblclickOptions' do not exist here.
dblclickHandleWith :: ElementHandle -> DblclickOptions -> Playwright ()
dblclickHandleWith handle opts = do
  timeout <-
    case dblclickOptionsTimeout opts of
      Just value ->
        pure value
      Nothing ->
        pure defaultTimeout
  _ :: ElementHandleDblclickResult <-
    handleSendWithTimeout
      handle
      "dblclick"
      timeout
      (mkElementHandleDblclickParams)
        { elementHandleDblclickParamsForce = dblclickOptionsForce opts
        , elementHandleDblclickParamsModifiers = fmap (fmap toHandleDblclickModifier) (dblclickOptionsModifiers opts)
        , elementHandleDblclickParamsPosition = fmap toProtocolPoint (dblclickOptionsPosition opts)
        , elementHandleDblclickParamsTrial = dblclickOptionsTrial opts
        }
  pure ()

toHandleDblclickModifier :: KeyboardModifier -> ElementHandleDblclickParamsModifiers
toHandleDblclickModifier modifier =
  case modifier of
    KeyboardModifierAlt ->
      ElementHandleDblclickParamsModifiersAlt
    KeyboardModifierControl ->
      ElementHandleDblclickParamsModifiersControl
    KeyboardModifierControlOrMeta ->
      ElementHandleDblclickParamsModifiersControlOrMeta
    KeyboardModifierMeta ->
      ElementHandleDblclickParamsModifiersMeta
    KeyboardModifierShift ->
      ElementHandleDblclickParamsModifiersShift

dispatchEventHandle :: ElementHandle -> DomEventType -> Playwright ()
dispatchEventHandle handle eventType =
  dispatchEventHandleWith handle eventType undefinedArgument

dispatchEventHandleWith :: ElementHandle -> DomEventType -> SerializedArgument -> Playwright ()
dispatchEventHandleWith handle eventType eventInit = do
  _ :: ElementHandleDispatchEventResult <-
    handleSend
      handle
      "dispatchEvent"
      (mkElementHandleDispatchEventParams eventInit (domEventTypeToText eventType))
  pure ()

toProtocolPoint :: Point -> Protocol.Point
toProtocolPoint point =
  Protocol.mkPoint (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point))

tapHandle :: ElementHandle -> Playwright ()
tapHandle handle =
  tapHandleWith handle defaultTapOptions

tapHandleWith :: ElementHandle -> TapOptions -> Playwright ()
tapHandleWith handle opts = do
  timeout <-
    case tapOptionsTimeout opts of
      Just value ->
        pure value
      Nothing ->
        pure defaultTimeout
  _ :: ElementHandleTapResult <-
    handleSendWithTimeout handle "tap" timeout
      (mkElementHandleTapParams)
        { elementHandleTapParamsForce = tapOptionsForce opts
        , elementHandleTapParamsModifiers = fmap (fmap toHandleTapModifier) (tapOptionsModifiers opts)
        , elementHandleTapParamsPosition = fmap toProtocolPoint (tapOptionsPosition opts)
        , elementHandleTapParamsTrial = tapOptionsTrial opts
        }
  pure ()

toHandleTapModifier :: KeyboardModifier -> ElementHandleTapParamsModifiers
toHandleTapModifier modifier =
  case modifier of
    KeyboardModifierAlt ->
      ElementHandleTapParamsModifiersAlt
    KeyboardModifierControl ->
      ElementHandleTapParamsModifiersControl
    KeyboardModifierControlOrMeta ->
      ElementHandleTapParamsModifiersControlOrMeta
    KeyboardModifierMeta ->
      ElementHandleTapParamsModifiersMeta
    KeyboardModifierShift ->
      ElementHandleTapParamsModifiersShift

screenshotHandle :: ElementHandle -> Playwright ByteString
screenshotHandle handle = do
  result <- handleSendWithTimeout handle "screenshot" defaultTimeout mkElementHandleScreenshotParams
  pure (protocolBinaryToByteString (elementHandleScreenshotResultBinary result))

-- | Screenshot with the given masks painted over; the options' 'screenshotOptionsMaskColor' and 'screenshotOptionsStyle' apply to them.
screenshotHandleWith :: ElementHandle -> [ElementHandleScreenshotParamsMask] -> ScreenshotOptions -> Playwright ByteString
screenshotHandleWith handle masks opts = do
  resolvedType <- either (throwIO . DecodeFailure) pure (resolveScreenshotType opts)
  result <-
    handleSendWithTimeout
      handle
      "screenshot"
      (screenshotOptionsTimeout opts)
      (mkElementHandleScreenshotParams)
        { elementHandleScreenshotParamsType = screenshotTypeParam <$> resolvedType
        , elementHandleScreenshotParamsQuality = screenshotOptionsQuality opts
        , elementHandleScreenshotParamsOmitBackground = screenshotOptionsOmitBackground opts
        , elementHandleScreenshotParamsCaret = screenshotCaretParam <$> screenshotOptionsCaret opts
        , elementHandleScreenshotParamsAnimations = screenshotAnimationsParam <$> screenshotOptionsAnimations opts
        , elementHandleScreenshotParamsScale = screenshotScaleParam <$> screenshotOptionsScale opts
        , elementHandleScreenshotParamsMask = if null masks then Nothing else Just masks
        , elementHandleScreenshotParamsMaskColor = screenshotOptionsMaskColor opts
        , elementHandleScreenshotParamsStyle = screenshotOptionsStyle opts
        }
  let bytes = protocolBinaryToByteString (elementHandleScreenshotResultBinary result)
  mapM_ (\filePath -> liftIO (writeScreenshotFile filePath bytes)) (screenshotOptionsPath opts)
  pure bytes
  where
    screenshotTypeParam screenshotType =
      case screenshotType of
        ScreenshotPng ->
          ElementHandleScreenshotParamsTypePng
        ScreenshotJpeg ->
          ElementHandleScreenshotParamsTypeJpeg
        ScreenshotWebp ->
          ElementHandleScreenshotParamsTypeWebp
    screenshotCaretParam screenshotCaret =
      case screenshotCaret of
        ScreenshotCaretHide ->
          ElementHandleScreenshotParamsCaretHide
        ScreenshotCaretInitial ->
          ElementHandleScreenshotParamsCaretInitial
    screenshotAnimationsParam screenshotAnimations =
      case screenshotAnimations of
        ScreenshotAnimationsDisabled ->
          ElementHandleScreenshotParamsAnimationsDisabled
        ScreenshotAnimationsAllow ->
          ElementHandleScreenshotParamsAnimationsAllow
    screenshotScaleParam screenshotScale =
      case screenshotScale of
        ScreenshotScaleCss ->
          ElementHandleScreenshotParamsScaleCss
        ScreenshotScaleDevice ->
          ElementHandleScreenshotParamsScaleDevice

boundingBox :: ElementHandle -> Playwright (Maybe BoundingBox)
boundingBox handle = do
  result <- handleSend handle "boundingBox" mkElementHandleBoundingBoxParams
  pure (fmap fromRect (elementHandleBoundingBoxResultValue result))

fromRect :: Rect -> BoundingBox
fromRect rect =
  BoundingBox
    { boundingBoxX = CssPixel (rectX rect)
    , boundingBoxY = CssPixel (rectY rect)
    , boundingBoxWidth = CssPixel (rectWidth rect)
    , boundingBoxHeight = CssPixel (rectHeight rect)
    }

innerTextHandle :: ElementHandle -> Playwright Text
innerTextHandle handle = do
  result <- handleSend handle "innerText" mkElementHandleInnerTextParams
  pure (elementHandleInnerTextResultValue result)

innerHtmlHandle :: ElementHandle -> Playwright Text
innerHtmlHandle handle = do
  result <- handleSend handle "innerHTML" mkElementHandleInnerHTMLParams
  pure (elementHandleInnerHTMLResultValue result)

textContentHandle :: ElementHandle -> Playwright (Maybe Text)
textContentHandle handle = do
  result <- handleSend handle "textContent" mkElementHandleTextContentParams
  pure (elementHandleTextContentResultValue result)

getAttributeHandle :: ElementHandle -> Text -> Playwright (Maybe Text)
getAttributeHandle handle name = do
  result <- handleSend handle "getAttribute" (mkElementHandleGetAttributeParams name)
  pure (elementHandleGetAttributeResultValue result)

inputValueHandle :: ElementHandle -> Playwright Text
inputValueHandle handle = do
  result <- handleSend handle "inputValue" mkElementHandleInputValueParams
  pure (elementHandleInputValueResultValue result)

-- | Checks the handle's checked state. Non-checkbox, non-radio elements fail
-- with the protocol's "Not a checkbox or radio button" error.
isCheckedHandle :: ElementHandle -> Playwright Bool
isCheckedHandle handle = do
  result <- handleSend handle "isChecked" mkElementHandleIsCheckedParams
  pure (elementHandleIsCheckedResultValue result)

isEditableHandle :: ElementHandle -> Playwright Bool
isEditableHandle handle = do
  result <- handleSend handle "isEditable" mkElementHandleIsEditableParams
  pure (elementHandleIsEditableResultValue result)

isEnabledHandle :: ElementHandle -> Playwright Bool
isEnabledHandle handle = do
  result <- handleSend handle "isEnabled" mkElementHandleIsEnabledParams
  pure (elementHandleIsEnabledResultValue result)

-- | The negation of 'isEnabledHandle', which is how the protocol defines
-- disabled elements.
isDisabledHandle :: ElementHandle -> Playwright Bool
isDisabledHandle handle = not <$> isEnabledHandle handle

isHiddenHandle :: ElementHandle -> Playwright Bool
isHiddenHandle handle = do
  result <- handleSend handle "isHidden" mkElementHandleIsHiddenParams
  pure (elementHandleIsHiddenResultValue result)

isVisibleHandle :: ElementHandle -> Playwright Bool
isVisibleHandle handle = do
  result <- handleSend handle "isVisible" mkElementHandleIsVisibleParams
  pure (elementHandleIsVisibleResultValue result)

contentFrameHandle :: ElementHandle -> Playwright (Maybe Frame)
contentFrameHandle handle = do
  result <- handleSend handle "contentFrame" mkElementHandleContentFrameParams
  case elementHandleContentFrameResultFrame result of
    Nothing ->
      pure Nothing
    Just ref ->
      Just <$> frameFromRef ref

ownerFrameHandle :: ElementHandle -> Playwright (Maybe Frame)
ownerFrameHandle handle = do
  result <- handleSend handle "ownerFrame" mkElementHandleOwnerFrameParams
  case elementHandleOwnerFrameResultFrame result of
    Nothing ->
      pure Nothing
    Just ref ->
      Just <$> frameFromRef ref

typeHandle :: ElementHandle -> Text -> Playwright ()
typeHandle handle text = do
  _ :: ElementHandleTypeResult <-
    handleSendWithTimeout handle "type" defaultTimeout (mkElementHandleTypeParams text)
  pure ()

scrollIntoViewIfNeeded :: ElementHandle -> Playwright ()
scrollIntoViewIfNeeded handle =
  scrollIntoViewIfNeededWith handle defaultTimeout

scrollIntoViewIfNeededWith :: ElementHandle -> Timeout -> Playwright ()
scrollIntoViewIfNeededWith handle timeout = do
  _ :: ElementHandleScrollIntoViewIfNeededResult <-
    handleSendWithTimeout handle "scrollIntoViewIfNeeded" timeout mkElementHandleScrollIntoViewIfNeededParams
  pure ()

selectText :: ElementHandle -> Playwright ()
selectText handle =
  selectTextWith handle defaultTimeout

selectTextWith :: ElementHandle -> Timeout -> Playwright ()
selectTextWith handle timeout = do
  _ :: ElementHandleSelectTextResult <-
    handleSendWithTimeout handle "selectText" timeout mkElementHandleSelectTextParams
  pure ()

setInputFilesHandle :: ElementHandle -> [InputFile] -> Playwright ()
setInputFilesHandle handle files = do
  _ :: ElementHandleSetInputFilesResult <-
    handleSendWithTimeout
      handle
      "setInputFiles"
      defaultTimeout
      (mkElementHandleSetInputFilesParams)
        { elementHandleSetInputFilesParamsPayloads = Just (fmap toPayload files)
        }
  pure ()

toPayload :: InputFile -> ElementHandleSetInputFilesParamsPayloads
toPayload file =
  mkElementHandleSetInputFilesParamsPayloads
    (protocolBinaryFromByteString (inputFileBytes file))
    (inputFileNameToText (inputFileName file))

waitForElementState :: ElementHandle -> ElementState -> Playwright ()
waitForElementState handle state =
  waitForElementStateWith handle state defaultTimeout

waitForElementStateWith :: ElementHandle -> ElementState -> Timeout -> Playwright ()
waitForElementStateWith handle state timeout = do
  _ :: ElementHandleWaitForElementStateResult <-
    handleSendWithTimeout
      handle
      "waitForElementState"
      timeout
      (mkElementHandleWaitForElementStateParams (toProtocolElementState state))
  pure ()

toProtocolElementState :: ElementState -> ElementHandleWaitForElementStateParamsState
toProtocolElementState state =
  case state of
    ElementVisible ->
      ElementHandleWaitForElementStateParamsStateVisible
    ElementHidden ->
      ElementHandleWaitForElementStateParamsStateHidden
    ElementStable ->
      ElementHandleWaitForElementStateParamsStateStable
    ElementEnabled ->
      ElementHandleWaitForElementStateParamsStateEnabled
    ElementDisabled ->
      ElementHandleWaitForElementStateParamsStateDisabled
    ElementEditable ->
      ElementHandleWaitForElementStateParamsStateEditable

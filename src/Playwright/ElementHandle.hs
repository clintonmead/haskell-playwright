module Playwright.ElementHandle
  ( ElementHandle (..)
  , boundingBox
  , clickHandle
  , clickHandleWith
  , dispatchEventHandle
  , dispatchEventHandleWith
  , tapHandle
  , elementArgument
  , contentFrameHandle
  , ownerFrameHandle
  , elementHandleFromRef
  , getAttributeHandle
  , innerHtmlHandle
  , innerTextHandle
  , inputValueHandle
  , isEditableHandle
  , isDisabledHandle
  , isEnabledHandle
  , isHiddenHandle
  , isVisibleHandle
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , screenshotHandle
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
import Playwright.Protocol
  ( ElementHandleBoundingBoxResult (elementHandleBoundingBoxResultValue)
  , ElementHandleClickParams (elementHandleClickParamsButton, elementHandleClickParamsClickCount, elementHandleClickParamsForce, elementHandleClickParamsModifiers, elementHandleClickParamsNoWaitAfter, elementHandleClickParamsPosition, elementHandleClickParamsSteps, elementHandleClickParamsTrial)
  , ElementHandleClickParamsButton (ElementHandleClickParamsButtonLeft, ElementHandleClickParamsButtonMiddle, ElementHandleClickParamsButtonRight)
  , ElementHandleClickParamsModifiers (ElementHandleClickParamsModifiersAlt, ElementHandleClickParamsModifiersControl, ElementHandleClickParamsModifiersControlOrMeta, ElementHandleClickParamsModifiersMeta, ElementHandleClickParamsModifiersShift)
  , ElementHandleClickResult
  , ElementHandleDispatchEventResult
  , ElementHandleContentFrameResult (elementHandleContentFrameResultFrame)
  , ElementHandleGetAttributeResult (elementHandleGetAttributeResultValue)
  , ElementHandleInnerHTMLResult (elementHandleInnerHTMLResultValue)
  , ElementHandleInnerTextResult (elementHandleInnerTextResultValue)
  , ElementHandleInputValueResult (elementHandleInputValueResultValue)
  , ElementHandleIsEditableResult (elementHandleIsEditableResultValue)
  , ElementHandleIsEnabledResult (elementHandleIsEnabledResultValue)
  , ElementHandleIsHiddenResult (elementHandleIsHiddenResultValue)
  , ElementHandleIsVisibleResult (elementHandleIsVisibleResultValue)
  , ElementHandleOwnerFrameResult (elementHandleOwnerFrameResultFrame)
  , ElementHandleScreenshotResult (elementHandleScreenshotResultBinary)
  , ElementHandleScrollIntoViewIfNeededResult
  , ElementHandleSelectTextResult
  , ElementHandleSetInputFilesParams (elementHandleSetInputFilesParamsPayloads)
  , ElementHandleSetInputFilesParamsPayloads
  , ElementHandleSetInputFilesResult
  , ElementHandleTapResult
  , ElementHandleTextContentResult (elementHandleTextContentResultValue)
  , ElementHandleTypeResult
  , ElementHandleWaitForElementStateParamsState (ElementHandleWaitForElementStateParamsStateDisabled, ElementHandleWaitForElementStateParamsStateEditable, ElementHandleWaitForElementStateParamsStateEnabled, ElementHandleWaitForElementStateParamsStateHidden, ElementHandleWaitForElementStateParamsStateStable, ElementHandleWaitForElementStateParamsStateVisible)
  , ElementHandleWaitForElementStateResult
  , Rect (rectHeight, rectWidth, rectX, rectY)
  , mkElementHandleBoundingBoxParams
  , mkElementHandleClickParams
  , mkElementHandleDispatchEventParams
  , mkElementHandleContentFrameParams
  , mkElementHandleGetAttributeParams
  , mkElementHandleInnerHTMLParams
  , mkElementHandleInnerTextParams
  , mkElementHandleInputValueParams
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
tapHandle handle = do
  _ :: ElementHandleTapResult <-
    handleSendWithTimeout handle "tap" defaultTimeout mkElementHandleTapParams
  pure ()

screenshotHandle :: ElementHandle -> Playwright ByteString
screenshotHandle handle = do
  result <- handleSendWithTimeout handle "screenshot" defaultTimeout mkElementHandleScreenshotParams
  pure (protocolBinaryToByteString (elementHandleScreenshotResultBinary result))

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

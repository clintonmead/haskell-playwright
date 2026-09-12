module Playwright.ElementHandle
  ( ElementHandle (..)
  , boundingBox
  , clickHandle
  , elementArgument
  , contentFrameHandle
  , ownerFrameHandle
  , elementHandleFromRef
  , getAttributeHandle
  , innerHtmlHandle
  , innerTextHandle
  , inputValueHandle
  , scrollIntoViewIfNeeded
  , scrollIntoViewIfNeededWith
  , selectText
  , selectTextWith
  , setInputFilesHandle
  , textContentHandle
  , typeHandle
  , waitForElementState
  , waitForElementStateWith
  ) where

import Data.Aeson
  ( FromJSON
  , ToJSON
  )
import Data.Text
  ( Text
  )
import Playwright.BoundingBox
  ( BoundingBox (BoundingBox, boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  )
import Playwright.Channel
  ( Channel (Channel, channelConnection, channelGuid)
  , ElementHandleTag
  , send
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.CssPixel
  ( CssPixel (CssPixel)
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
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ElementHandleBoundingBoxResult (elementHandleBoundingBoxResultValue)
  , ElementHandleClickResult
  , ElementHandleContentFrameResult (elementHandleContentFrameResultFrame)
  , ElementHandleGetAttributeResult (elementHandleGetAttributeResultValue)
  , ElementHandleInnerHTMLResult (elementHandleInnerHTMLResultValue)
  , ElementHandleInnerTextResult (elementHandleInnerTextResultValue)
  , ElementHandleInputValueResult (elementHandleInputValueResultValue)
  , ElementHandleOwnerFrameResult (elementHandleOwnerFrameResultFrame)
  , ElementHandleScrollIntoViewIfNeededResult
  , ElementHandleSelectTextResult
  , ElementHandleSetInputFilesParams (elementHandleSetInputFilesParamsPayloads)
  , ElementHandleSetInputFilesParamsPayloads
  , ElementHandleSetInputFilesResult
  , ElementHandleTextContentResult (elementHandleTextContentResultValue)
  , ElementHandleTypeResult
  , ElementHandleWaitForElementStateParamsState (ElementHandleWaitForElementStateParamsStateDisabled, ElementHandleWaitForElementStateParamsStateEditable, ElementHandleWaitForElementStateParamsStateEnabled, ElementHandleWaitForElementStateParamsStateHidden, ElementHandleWaitForElementStateParamsStateStable, ElementHandleWaitForElementStateParamsStateVisible)
  , ElementHandleWaitForElementStateResult
  , Rect (rectHeight, rectWidth, rectX, rectY)
  , mkElementHandleBoundingBoxParams
  , mkElementHandleClickParams
  , mkElementHandleContentFrameParams
  , mkElementHandleGetAttributeParams
  , mkElementHandleInnerHTMLParams
  , mkElementHandleInnerTextParams
  , mkElementHandleInputValueParams
  , mkElementHandleOwnerFrameParams
  , mkElementHandleScrollIntoViewIfNeededParams
  , mkElementHandleSelectTextParams
  , mkElementHandleSetInputFilesParams
  , mkElementHandleSetInputFilesParamsPayloads
  , mkElementHandleTextContentParams
  , mkElementHandleTypeParams
  , mkElementHandleWaitForElementStateParams
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , channelArgument
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )

newtype ElementHandle = ElementHandle
  { elementHandleChannel :: Channel ElementHandleTag
  }

elementHandleFromRef :: Connection -> ChannelRef -> ElementHandle
elementHandleFromRef conn ref =
  ElementHandle (Channel conn (channelRefGuid ref))

elementArgument :: ElementHandle -> SerializedArgument
elementArgument handle =
  channelArgument (ChannelRef (channelGuid (elementHandleChannel handle)))

handleSend :: (ToJSON params, FromJSON result) => ElementHandle -> Text -> params -> IO result
handleSend handle method =
  send (elementHandleChannel handle) (MethodName method)

clickHandle :: ElementHandle -> IO ()
clickHandle handle = do
  _ :: ElementHandleClickResult <-
    handleSend handle "click" (mkElementHandleClickParams defaultTimeout)
  pure ()

boundingBox :: ElementHandle -> IO (Maybe BoundingBox)
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

innerTextHandle :: ElementHandle -> IO Text
innerTextHandle handle = do
  result <- handleSend handle "innerText" mkElementHandleInnerTextParams
  pure (elementHandleInnerTextResultValue result)

innerHtmlHandle :: ElementHandle -> IO Text
innerHtmlHandle handle = do
  result <- handleSend handle "innerHTML" mkElementHandleInnerHTMLParams
  pure (elementHandleInnerHTMLResultValue result)

textContentHandle :: ElementHandle -> IO (Maybe Text)
textContentHandle handle = do
  result <- handleSend handle "textContent" mkElementHandleTextContentParams
  pure (elementHandleTextContentResultValue result)

getAttributeHandle :: ElementHandle -> Text -> IO (Maybe Text)
getAttributeHandle handle name = do
  result <- handleSend handle "getAttribute" (mkElementHandleGetAttributeParams name)
  pure (elementHandleGetAttributeResultValue result)

inputValueHandle :: ElementHandle -> IO Text
inputValueHandle handle = do
  result <- handleSend handle "inputValue" mkElementHandleInputValueParams
  pure (elementHandleInputValueResultValue result)

contentFrameHandle :: ElementHandle -> IO (Maybe Frame)
contentFrameHandle handle = do
  result <- handleSend handle "contentFrame" mkElementHandleContentFrameParams
  case elementHandleContentFrameResultFrame result of
    Nothing ->
      pure Nothing
    Just ref ->
      Just <$> frameFromRef (channelConnection (elementHandleChannel handle)) ref

ownerFrameHandle :: ElementHandle -> IO (Maybe Frame)
ownerFrameHandle handle = do
  result <- handleSend handle "ownerFrame" mkElementHandleOwnerFrameParams
  case elementHandleOwnerFrameResultFrame result of
    Nothing ->
      pure Nothing
    Just ref ->
      Just <$> frameFromRef (channelConnection (elementHandleChannel handle)) ref

typeHandle :: ElementHandle -> Text -> IO ()
typeHandle handle text = do
  _ :: ElementHandleTypeResult <-
    handleSend handle "type" (mkElementHandleTypeParams text defaultTimeout)
  pure ()

scrollIntoViewIfNeeded :: ElementHandle -> IO ()
scrollIntoViewIfNeeded handle =
  scrollIntoViewIfNeededWith handle defaultTimeout

scrollIntoViewIfNeededWith :: ElementHandle -> Timeout -> IO ()
scrollIntoViewIfNeededWith handle timeout = do
  _ :: ElementHandleScrollIntoViewIfNeededResult <-
    handleSend handle "scrollIntoViewIfNeeded" (mkElementHandleScrollIntoViewIfNeededParams timeout)
  pure ()

selectText :: ElementHandle -> IO ()
selectText handle =
  selectTextWith handle defaultTimeout

selectTextWith :: ElementHandle -> Timeout -> IO ()
selectTextWith handle timeout = do
  _ :: ElementHandleSelectTextResult <-
    handleSend handle "selectText" (mkElementHandleSelectTextParams timeout)
  pure ()

setInputFilesHandle :: ElementHandle -> [InputFile] -> IO ()
setInputFilesHandle handle files = do
  _ :: ElementHandleSetInputFilesResult <-
    handleSend
      handle
      "setInputFiles"
      (mkElementHandleSetInputFilesParams defaultTimeout)
        { elementHandleSetInputFilesParamsPayloads = Just (fmap toPayload files)
        }
  pure ()

toPayload :: InputFile -> ElementHandleSetInputFilesParamsPayloads
toPayload file =
  mkElementHandleSetInputFilesParamsPayloads
    (protocolBinaryFromByteString (inputFileBytes file))
    (inputFileNameToText (inputFileName file))

waitForElementState :: ElementHandle -> ElementState -> IO ()
waitForElementState handle state =
  waitForElementStateWith handle state defaultTimeout

waitForElementStateWith :: ElementHandle -> ElementState -> Timeout -> IO ()
waitForElementStateWith handle state timeout = do
  _ :: ElementHandleWaitForElementStateResult <-
    handleSend
      handle
      "waitForElementState"
      (mkElementHandleWaitForElementStateParams (toProtocolElementState state) timeout)
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

module Playwright.ElementHandle
  ( ElementHandle (..)
  , boundingBox
  , clickHandle
  , elementArgument
  , elementHandleFromRef
  , getAttributeHandle
  , innerHtmlHandle
  , innerTextHandle
  , inputValueHandle
  , textContentHandle
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
  ( Channel (Channel, channelGuid)
  , ElementHandleTag
  , send
  )
import Playwright.Connection
  ( Connection
  )
import Playwright.CssPixel
  ( CssPixel (CssPixel)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (ChannelRef, channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ElementHandleBoundingBoxResult (elementHandleBoundingBoxResultValue)
  , ElementHandleClickResult
  , ElementHandleGetAttributeResult (elementHandleGetAttributeResultValue)
  , ElementHandleInnerHTMLResult (elementHandleInnerHTMLResultValue)
  , ElementHandleInnerTextResult (elementHandleInnerTextResultValue)
  , ElementHandleInputValueResult (elementHandleInputValueResultValue)
  , ElementHandleTextContentResult (elementHandleTextContentResultValue)
  , Rect (rectHeight, rectWidth, rectX, rectY)
  , mkElementHandleBoundingBoxParams
  , mkElementHandleClickParams
  , mkElementHandleGetAttributeParams
  , mkElementHandleInnerHTMLParams
  , mkElementHandleInnerTextParams
  , mkElementHandleInputValueParams
  , mkElementHandleTextContentParams
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  , channelArgument
  )
import Playwright.Timeout
  ( defaultTimeout
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

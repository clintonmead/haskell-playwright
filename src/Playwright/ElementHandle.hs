module Playwright.ElementHandle
  ( ElementHandle (..)
  , boundingBox
  , elementHandleFromRef
  ) where

import Playwright.BoundingBox
  ( BoundingBox (BoundingBox, boundingBoxHeight, boundingBoxWidth, boundingBoxX, boundingBoxY)
  )
import Playwright.Channel
  ( Channel (Channel)
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
  ( ChannelRef (channelRefGuid)
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Protocol
  ( ElementHandleBoundingBoxResult (elementHandleBoundingBoxResultValue)
  , Rect (rectHeight, rectWidth, rectX, rectY)
  , mkElementHandleBoundingBoxParams
  )

newtype ElementHandle = ElementHandle
  { elementHandleChannel :: Channel ElementHandleTag
  }

elementHandleFromRef :: Connection -> ChannelRef -> ElementHandle
elementHandleFromRef conn ref =
  ElementHandle (Channel conn (channelRefGuid ref))

boundingBox :: ElementHandle -> IO (Maybe BoundingBox)
boundingBox handle = do
  result <-
    send
      (elementHandleChannel handle)
      (MethodName "boundingBox")
      mkElementHandleBoundingBoxParams
  pure (fmap fromRect (elementHandleBoundingBoxResultValue result))

fromRect :: Rect -> BoundingBox
fromRect rect =
  BoundingBox
    { boundingBoxX = CssPixel (rectX rect)
    , boundingBoxY = CssPixel (rectY rect)
    , boundingBoxWidth = CssPixel (rectWidth rect)
    , boundingBoxHeight = CssPixel (rectHeight rect)
    }

module Playwright.RequestFrame
  ( requestFrame
  , responseFrame
  ) where

import Playwright.Channel
  ( Channel (channelConnection)
  )
import Playwright.Frame
  ( Frame
  , frameFromRef
  )
import Playwright.Request
  ( Request (requestChannel)
  , requestFrameRef
  )
import Playwright.Response
  ( Response
  , responseRequest
  )

requestFrame :: Request -> IO (Maybe Frame)
requestFrame request = do
  ref <- requestFrameRef request
  case ref of
    Nothing ->
      pure Nothing
    Just frameRef ->
      Just <$> frameFromRef (channelConnection (requestChannel request)) frameRef

responseFrame :: Response -> IO (Maybe Frame)
responseFrame response =
  requestFrame =<< responseRequest response

module Playwright.RequestFrame
  ( requestFrame
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

requestFrame :: Request -> IO (Maybe Frame)
requestFrame request = do
  ref <- requestFrameRef request
  case ref of
    Nothing ->
      pure Nothing
    Just frameRef ->
      Just <$> frameFromRef (channelConnection (requestChannel request)) frameRef

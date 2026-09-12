module Playwright.RequestFrame
  ( requestFrame
  , responseFrame
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Frame
  ( Frame
  , frameFromRef
  )
import Playwright.Request
  ( Request
  , requestFrameRef
  )
import Playwright.Response
  ( Response
  , responseRequest
  )

requestFrame :: Request -> Playwright (Maybe Frame)
requestFrame request = do
  ref <- requestFrameRef request
  case ref of
    Nothing ->
      pure Nothing
    Just frameRef ->
      Just <$> frameFromRef frameRef

responseFrame :: Response -> Playwright (Maybe Frame)
responseFrame response =
  requestFrame =<< responseRequest response

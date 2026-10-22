module Playwright.Touchscreen
  ( touchscreenTap
  ) where

import Playwright.Monad
  ( Playwright
  )
import Playwright.Channel
  ( send
  )
import Playwright.CssPixel
  ( cssPixelToScientific
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Point
  ( Point (pointX, pointY)
  )
import Playwright.Protocol
  ( PageTouchscreenTapResult
  , mkPageTouchscreenTapParams
  )

-- | Dispatches a touch tap at the given viewport coordinates, the same as
-- @page.touchscreen.tap(x, y)@ in the JavaScript client.
touchscreenTap :: Page -> Point -> Playwright ()
touchscreenTap page point = do
  _ :: PageTouchscreenTapResult <-
    send
      (pageChannel page)
      (MethodName "touchscreenTap")
      (mkPageTouchscreenTapParams (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point)))
  pure ()

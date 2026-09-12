module Playwright.Mouse
  ( mouseClick
  , mouseDown
  , mouseMove
  , mouseUp
  , mouseWheel
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
  ( PageMouseClickResult
  , PageMouseDownResult
  , PageMouseMoveResult
  , PageMouseUpResult
  , PageMouseWheelResult
  , mkPageMouseClickParams
  , mkPageMouseDownParams
  , mkPageMouseMoveParams
  , mkPageMouseUpParams
  , mkPageMouseWheelParams
  )
import Playwright.WheelDelta
  ( WheelDelta (wheelDeltaToScientific)
  )

mouseClick :: Page -> Point -> Playwright ()
mouseClick page point = do
  _ :: PageMouseClickResult <-
    send
      (pageChannel page)
      (MethodName "mouseClick")
      (mkPageMouseClickParams (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point)))
  pure ()

mouseMove :: Page -> Point -> Playwright ()
mouseMove page point = do
  _ :: PageMouseMoveResult <-
    send
      (pageChannel page)
      (MethodName "mouseMove")
      (mkPageMouseMoveParams (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point)))
  pure ()

mouseDown :: Page -> Playwright ()
mouseDown page = do
  _ :: PageMouseDownResult <-
    send (pageChannel page) (MethodName "mouseDown") mkPageMouseDownParams
  pure ()

mouseUp :: Page -> Playwright ()
mouseUp page = do
  _ :: PageMouseUpResult <-
    send (pageChannel page) (MethodName "mouseUp") mkPageMouseUpParams
  pure ()

mouseWheel :: Page -> WheelDelta -> WheelDelta -> Playwright ()
mouseWheel page deltaX deltaY = do
  _ :: PageMouseWheelResult <-
    send
      (pageChannel page)
      (MethodName "mouseWheel")
      (mkPageMouseWheelParams (wheelDeltaToScientific deltaX) (wheelDeltaToScientific deltaY))
  pure ()

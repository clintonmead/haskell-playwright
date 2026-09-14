module Playwright.Mouse
  ( mouseClick
  , mouseClickWith
  , mouseDblclick
  , mouseDown
  , mouseDownWith
  , mouseMove
  , mouseMoveWith
  , mouseUp
  , mouseUpWith
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
import Playwright.MouseButton
  ( MouseButton (MouseButtonLeft, MouseButtonMiddle, MouseButtonRight)
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Point
  ( Point (pointX, pointY)
  )
import Playwright.Protocol
  ( PageMouseClickParams (pageMouseClickParamsButton, pageMouseClickParamsClickCount)
  , PageMouseClickParamsButton (PageMouseClickParamsButtonLeft, PageMouseClickParamsButtonMiddle, PageMouseClickParamsButtonRight)
  , PageMouseClickResult
  , PageMouseDownParams (pageMouseDownParamsButton)
  , PageMouseDownParamsButton (PageMouseDownParamsButtonLeft, PageMouseDownParamsButtonMiddle, PageMouseDownParamsButtonRight)
  , PageMouseDownResult
  , PageMouseMoveParams (pageMouseMoveParamsSteps)
  , PageMouseMoveResult
  , PageMouseUpParams (pageMouseUpParamsButton)
  , PageMouseUpParamsButton (PageMouseUpParamsButtonLeft, PageMouseUpParamsButtonMiddle, PageMouseUpParamsButtonRight)
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
mouseClick page point =
  mouseClickWith page point Nothing Nothing

mouseDblclick :: Page -> Point -> Playwright ()
mouseDblclick page point =
  mouseClickWith page point Nothing (Just 2)

mouseClickWith :: Page -> Point -> Maybe MouseButton -> Maybe Int -> Playwright ()
mouseClickWith page point button clickCount = do
  _ :: PageMouseClickResult <-
    send
      (pageChannel page)
      (MethodName "mouseClick")
      (mkPageMouseClickParams (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point)))
        { pageMouseClickParamsButton = fmap toClickButton button
        , pageMouseClickParamsClickCount = clickCount
        }
  pure ()

mouseMove :: Page -> Point -> Playwright ()
mouseMove page point =
  mouseMoveWith page point Nothing

mouseMoveWith :: Page -> Point -> Maybe Int -> Playwright ()
mouseMoveWith page point steps = do
  _ :: PageMouseMoveResult <-
    send
      (pageChannel page)
      (MethodName "mouseMove")
      (mkPageMouseMoveParams (cssPixelToScientific (pointX point)) (cssPixelToScientific (pointY point)))
        { pageMouseMoveParamsSteps = steps
        }
  pure ()

mouseDown :: Page -> Playwright ()
mouseDown page =
  mouseDownWith page Nothing

mouseDownWith :: Page -> Maybe MouseButton -> Playwright ()
mouseDownWith page button = do
  _ :: PageMouseDownResult <-
    send
      (pageChannel page)
      (MethodName "mouseDown")
      mkPageMouseDownParams {pageMouseDownParamsButton = fmap toDownButton button}
  pure ()

mouseUp :: Page -> Playwright ()
mouseUp page =
  mouseUpWith page Nothing

mouseUpWith :: Page -> Maybe MouseButton -> Playwright ()
mouseUpWith page button = do
  _ :: PageMouseUpResult <-
    send
      (pageChannel page)
      (MethodName "mouseUp")
      mkPageMouseUpParams {pageMouseUpParamsButton = fmap toUpButton button}
  pure ()

mouseWheel :: Page -> WheelDelta -> WheelDelta -> Playwright ()
mouseWheel page deltaX deltaY = do
  _ :: PageMouseWheelResult <-
    send
      (pageChannel page)
      (MethodName "mouseWheel")
      (mkPageMouseWheelParams (wheelDeltaToScientific deltaX) (wheelDeltaToScientific deltaY))
  pure ()

toClickButton :: MouseButton -> PageMouseClickParamsButton
toClickButton button =
  case button of
    MouseButtonLeft ->
      PageMouseClickParamsButtonLeft
    MouseButtonRight ->
      PageMouseClickParamsButtonRight
    MouseButtonMiddle ->
      PageMouseClickParamsButtonMiddle

toDownButton :: MouseButton -> PageMouseDownParamsButton
toDownButton button =
  case button of
    MouseButtonLeft ->
      PageMouseDownParamsButtonLeft
    MouseButtonRight ->
      PageMouseDownParamsButtonRight
    MouseButtonMiddle ->
      PageMouseDownParamsButtonMiddle

toUpButton :: MouseButton -> PageMouseUpParamsButton
toUpButton button =
  case button of
    MouseButtonLeft ->
      PageMouseUpParamsButtonLeft
    MouseButtonRight ->
      PageMouseUpParamsButtonRight
    MouseButtonMiddle ->
      PageMouseUpParamsButtonMiddle

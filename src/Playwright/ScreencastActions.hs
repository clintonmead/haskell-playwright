module Playwright.ScreencastActions
  ( ActionCursor (..)
  , ActionPosition (..)
  , ActionStyle (..)
  , ScreencastActionsHandle
  , ScreencastActionsOptions (..)
  , defaultActionStyle
  , defaultScreencastActionsOptions
  , screencastActionsDispose
  , screencastHideActions
  , screencastShowActions
  ) where

import Data.Scientific
  ( fromFloatDigits
  )
import Data.Text
  ( Text
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page (pageChannel)
  )
import Playwright.Protocol qualified as P
import Playwright.Channel
  ( send
  )

-- | Where the action title is placed on the page.
data ActionPosition
  = PositionTopLeft
  | PositionTop
  | PositionTopRight
  | PositionBottomLeft
  | PositionBottom
  | PositionBottomRight
  deriving stock (Eq, Show)

-- | The cursor decoration drawn for pointer actions.
data ActionCursor
  = -- | Pointer cursor that animates between action points (the default).
    CursorPointer
  | -- | No cursor decoration; the click marker still renders.
    CursorNone
  deriving stock (Eq, Show)

-- | CSS declarations for the annotation parts. A part that is 'Nothing' is
-- left to the driver's default look.
data ActionStyle = ActionStyle
  { actionStylePoint :: Maybe Text
  , actionStyleHighlight :: Maybe Text
  , actionStyleTitle :: Maybe Text
  }
  deriving stock (Eq, Show)

defaultActionStyle :: ActionStyle
defaultActionStyle =
  ActionStyle
    { actionStylePoint = Nothing
    , actionStyleHighlight = Nothing
    , actionStyleTitle = Nothing
    }

data ScreencastActionsOptions = ScreencastActionsOptions
  { screencastActionsOptionsCursor :: Maybe ActionCursor
  , screencastActionsOptionsDuration :: Maybe Double
  -- ^ How long each annotation stays visible, in milliseconds.
  , screencastActionsOptionsFontSize :: Maybe Int
  , screencastActionsOptionsPosition :: Maybe ActionPosition
  , screencastActionsOptionsStyle :: Maybe ActionStyle
  }
  deriving stock (Eq, Show)

defaultScreencastActionsOptions :: ScreencastActionsOptions
defaultScreencastActionsOptions =
  ScreencastActionsOptions
    { screencastActionsOptionsCursor = Nothing
    , screencastActionsOptionsDuration = Nothing
    , screencastActionsOptionsFontSize = Nothing
    , screencastActionsOptionsPosition = Nothing
    , screencastActionsOptionsStyle = Nothing
    }

-- | Handle returned by 'screencastShowActions'. Disposing it removes the
-- annotations, like 'screencastHideActions'.
newtype ScreencastActionsHandle = ScreencastActionsHandle Page

-- | Enables visual annotations on the elements the page interacts with.
screencastShowActions :: Page -> ScreencastActionsOptions -> Playwright ScreencastActionsHandle
screencastShowActions page options = do
  _ :: P.PageScreencastShowActionsResult <-
    send (pageChannel page) (MethodName "screencastShowActions") (toParams options)
  pure (ScreencastActionsHandle page)

-- | Removes the action annotations.
screencastHideActions :: Page -> Playwright ()
screencastHideActions page = do
  _ :: P.PageScreencastHideActionsResult <-
    send (pageChannel page) (MethodName "screencastHideActions") P.mkPageScreencastHideActionsParams
  pure ()

-- | Stops showing actions, as 'screencastHideActions' does.
screencastActionsDispose :: ScreencastActionsHandle -> Playwright ()
screencastActionsDispose (ScreencastActionsHandle page) =
  screencastHideActions page

toParams :: ScreencastActionsOptions -> P.PageScreencastShowActionsParams
toParams options =
  P.PageScreencastShowActionsParams
    { P.pageScreencastShowActionsParamsCursor = cursorParam <$> screencastActionsOptionsCursor options
    , P.pageScreencastShowActionsParamsDuration = fromFloatDigits <$> screencastActionsOptionsDuration options
    , P.pageScreencastShowActionsParamsFontSize = screencastActionsOptionsFontSize options
    , P.pageScreencastShowActionsParamsPosition = positionParam <$> screencastActionsOptionsPosition options
    , P.pageScreencastShowActionsParamsStyle = styleParam <$> screencastActionsOptionsStyle options
    }

cursorParam :: ActionCursor -> P.PageScreencastShowActionsParamsCursor
cursorParam CursorPointer = P.PageScreencastShowActionsParamsCursorPointer
cursorParam CursorNone = P.PageScreencastShowActionsParamsCursorNone

positionParam :: ActionPosition -> P.PageScreencastShowActionsParamsPosition
positionParam position =
  case position of
    PositionTopLeft -> P.PageScreencastShowActionsParamsPositionTopLeft
    PositionTop -> P.PageScreencastShowActionsParamsPositionTop
    PositionTopRight -> P.PageScreencastShowActionsParamsPositionTopRight
    PositionBottomLeft -> P.PageScreencastShowActionsParamsPositionBottomLeft
    PositionBottom -> P.PageScreencastShowActionsParamsPositionBottom
    PositionBottomRight -> P.PageScreencastShowActionsParamsPositionBottomRight

styleParam :: ActionStyle -> P.PageScreencastShowActionsParamsStyle
styleParam style =
  P.PageScreencastShowActionsParamsStyle
    { P.pageScreencastShowActionsParamsStylePoint = actionStylePoint style
    , P.pageScreencastShowActionsParamsStyleHighlight = actionStyleHighlight style
    , P.pageScreencastShowActionsParamsStyleTitle = actionStyleTitle style
    }

module Playwright.ScreencastOverlay
  ( ScreencastOverlayHandle
  , ScreencastOverlayOptions (..)
  , defaultScreencastOverlayOptions
  , screencastHideOverlays
  , screencastOverlayDispose
  , screencastShowOverlay
  , screencastShowOverlays
  ) where

import Data.Scientific
  ( fromFloatDigits
  )
import Data.Text
  ( Text
  )
import Playwright.Channel
  ( send
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

data ScreencastOverlayOptions = ScreencastOverlayOptions
  { screencastOverlayOptionsDuration :: Maybe Double
  -- ^ How long the overlay stays visible, in milliseconds. Without it the
  -- overlay stays until it is disposed.
  }
  deriving stock (Eq, Show)

defaultScreencastOverlayOptions :: ScreencastOverlayOptions
defaultScreencastOverlayOptions =
  ScreencastOverlayOptions
    { screencastOverlayOptionsDuration = Nothing
    }

-- | Handle returned by 'screencastShowOverlay'. Holds the overlay id so that
-- 'screencastOverlayDispose' can remove exactly this overlay.
data ScreencastOverlayHandle = ScreencastOverlayHandle
  { overlayHandlePage :: Page
  , overlayHandleId :: Text
  }

-- | Adds an HTML overlay to the screencast. Scripts and event handlers in the
-- HTML are stripped by the driver.
screencastShowOverlay :: Page -> Text -> ScreencastOverlayOptions -> Playwright ScreencastOverlayHandle
screencastShowOverlay page html options = do
  result :: P.PageScreencastShowOverlayResult <-
    send (pageChannel page) (MethodName "screencastShowOverlay") (toShowParams html options)
  pure
    ScreencastOverlayHandle
      { overlayHandlePage = page
      , overlayHandleId = P.pageScreencastShowOverlayResultId result
      }

-- | Removes the overlay created by 'screencastShowOverlay'.
screencastOverlayDispose :: ScreencastOverlayHandle -> Playwright ()
screencastOverlayDispose handle = do
  _ :: P.PageScreencastRemoveOverlayResult <-
    send
      (pageChannel (overlayHandlePage handle))
      (MethodName "screencastRemoveOverlay")
      P.PageScreencastRemoveOverlayParams
        { P.pageScreencastRemoveOverlayParamsId = overlayHandleId handle
        }
  pure ()

-- | Hides all overlays without removing them. See 'screencastShowOverlays'.
screencastHideOverlays :: Page -> Playwright ()
screencastHideOverlays page =
  setOverlayVisible page False

-- | Shows overlays that were hidden with 'screencastHideOverlays'.
screencastShowOverlays :: Page -> Playwright ()
screencastShowOverlays page =
  setOverlayVisible page True

setOverlayVisible :: Page -> Bool -> Playwright ()
setOverlayVisible page visible = do
  _ :: P.PageScreencastSetOverlayVisibleResult <-
    send
      (pageChannel page)
      (MethodName "screencastSetOverlayVisible")
      P.PageScreencastSetOverlayVisibleParams
        { P.pageScreencastSetOverlayVisibleParamsVisible = visible
        }
  pure ()

toShowParams :: Text -> ScreencastOverlayOptions -> P.PageScreencastShowOverlayParams
toShowParams html options =
  P.PageScreencastShowOverlayParams
    { P.pageScreencastShowOverlayParamsDuration = fromFloatDigits <$> screencastOverlayOptionsDuration options
    , P.pageScreencastShowOverlayParamsHtml = html
    }

module Playwright.Locator
  ( Locator (..)
  , check
  , checkWith
  , childLocator
  , click
  , dblclick
  , fill
  , focus
  , getAttribute
  , hover
  , innerHtml
  , innerText
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , locatorCount
  , press
  , setChecked
  , textContent
  , typeText
  , uncheck
  , uncheckWith
  ) where

import Data.Text
  ( Text
  )
import Playwright.CheckOptions
  ( CheckOptions
  , defaultCheckOptions
  )
import Playwright.Frame
  ( Frame
  )
import Playwright.Frame.Action
  ( checkFrame
  , clickFrame
  , dblclickFrame
  , fillFrame
  , focusFrame
  , hoverFrame
  , pressFrame
  , typeTextFrame
  , uncheckFrame
  )
import Playwright.Frame.Inspect
  ( getAttributeFrame
  , innerHtmlFrame
  , innerTextFrame
  , inputValueFrame
  , isCheckedFrame
  , isDisabledFrame
  , isEditableFrame
  , isEnabledFrame
  , isHiddenFrame
  , isVisibleFrame
  , queryCountFrame
  , textContentFrame
  )
import Playwright.Key
  ( Key
  )
import Playwright.Selector
  ( Selector
  , nestedSelector
  )
import Playwright.Timeout
  ( defaultTimeout
  )

data Locator = Locator
  { locatorFrame :: Frame
  , locatorSelector :: Selector
  }

childLocator :: Locator -> Selector -> Locator
childLocator parent child =
  Locator
    { locatorFrame = locatorFrame parent
    , locatorSelector = nestedSelector (locatorSelector parent) child
    }

click :: Locator -> IO ()
click loc =
  clickFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

dblclick :: Locator -> IO ()
dblclick loc =
  dblclickFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

fill :: Locator -> Text -> IO ()
fill loc value =
  fillFrame (locatorFrame loc) (locatorSelector loc) value defaultTimeout

check :: Locator -> IO ()
check loc =
  checkWith loc defaultCheckOptions

checkWith :: Locator -> CheckOptions -> IO ()
checkWith loc =
  checkFrame (locatorFrame loc) (locatorSelector loc)

uncheck :: Locator -> IO ()
uncheck loc =
  uncheckWith loc defaultCheckOptions

uncheckWith :: Locator -> CheckOptions -> IO ()
uncheckWith loc =
  uncheckFrame (locatorFrame loc) (locatorSelector loc)

setChecked :: Locator -> Bool -> IO ()
setChecked loc wanted =
  if wanted
    then check loc
    else uncheck loc

focus :: Locator -> IO ()
focus loc =
  focusFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

hover :: Locator -> IO ()
hover loc =
  hoverFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

press :: Locator -> Key -> IO ()
press loc key =
  pressFrame (locatorFrame loc) (locatorSelector loc) key defaultTimeout

typeText :: Locator -> Text -> IO ()
typeText loc text =
  typeTextFrame (locatorFrame loc) (locatorSelector loc) text defaultTimeout

innerText :: Locator -> IO Text
innerText loc =
  innerTextFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

innerHtml :: Locator -> IO Text
innerHtml loc =
  innerHtmlFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

textContent :: Locator -> IO (Maybe Text)
textContent loc =
  textContentFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

getAttribute :: Locator -> Text -> IO (Maybe Text)
getAttribute loc name =
  getAttributeFrame (locatorFrame loc) (locatorSelector loc) name defaultTimeout

inputValue :: Locator -> IO Text
inputValue loc =
  inputValueFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

isVisible :: Locator -> IO Bool
isVisible loc =
  isVisibleFrame (locatorFrame loc) (locatorSelector loc)

isHidden :: Locator -> IO Bool
isHidden loc =
  isHiddenFrame (locatorFrame loc) (locatorSelector loc)

isChecked :: Locator -> IO Bool
isChecked loc =
  isCheckedFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

isEnabled :: Locator -> IO Bool
isEnabled loc =
  isEnabledFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

isDisabled :: Locator -> IO Bool
isDisabled loc =
  isDisabledFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

isEditable :: Locator -> IO Bool
isEditable loc =
  isEditableFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

locatorCount :: Locator -> IO Int
locatorCount loc =
  queryCountFrame (locatorFrame loc) (locatorSelector loc)

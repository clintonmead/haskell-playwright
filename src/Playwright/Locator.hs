module Playwright.Locator
  ( Locator (..)
  , check
  , checkWith
  , childLocator
  , click
  , dblclick
  , dispatchEvent
  , elementHandle
  , fill
  , first
  , focus
  , getAttribute
  , getByLabel
  , getByRole
  , getByTestId
  , getByText
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
  , lastLocator
  , locatorCount
  , nth
  , press
  , selectOption
  , setChecked
  , textContent
  , typeText
  , uncheck
  , uncheckWith
  , waitFor
  , waitForWith
  ) where

import Data.Text
  ( Text
  , pack
  )
import Playwright.AriaRole
  ( AriaRole
  )
import Playwright.CheckOptions
  ( CheckOptions
  , defaultCheckOptions
  )
import Playwright.DomEventType
  ( DomEventType
  )
import Playwright.ElementHandle
  ( ElementHandle
  )
import Playwright.Exactness
  ( Exactness
  )
import Playwright.Frame
  ( Frame
  )
import Playwright.Frame.Action
  ( checkFrame
  , clickFrame
  , dblclickFrame
  , dispatchEventFrame
  , fillFrame
  , focusFrame
  , hoverFrame
  , pressFrame
  , selectOptionFrame
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
  , querySelectorFrame
  , textContentFrame
  )
import Playwright.Frame.Wait
  ( waitForSelectorFrame
  )
import Playwright.GetBy
  ( getByLabelSelector
  , getByRoleSelector
  , getByTestIdSelector
  , getByTextSelector
  )
import Playwright.GetByRoleOptions
  ( GetByRoleOptions
  )
import Playwright.Key
  ( Key
  )
import Playwright.SelectOption
  ( SelectOption
  )
import Playwright.Selector
  ( Selector
  , nestedSelector
  , selectorFromText
  )
import Playwright.Timeout
  ( Timeout
  , defaultTimeout
  )
import Playwright.WaitForState
  ( WaitForState (WaitForAttached)
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

nth :: Locator -> Int -> Locator
nth loc index =
  childLocator loc (selectorFromText ("nth=" <> pack (show index)))

first :: Locator -> Locator
first loc =
  nth loc 0

lastLocator :: Locator -> Locator
lastLocator loc =
  nth loc (-1)

getByText :: Locator -> Text -> Exactness -> Locator
getByText loc text exactness =
  childLocator loc (getByTextSelector text exactness)

getByLabel :: Locator -> Text -> Exactness -> Locator
getByLabel loc text exactness =
  childLocator loc (getByLabelSelector text exactness)

getByTestId :: Locator -> Text -> Locator
getByTestId loc testId =
  childLocator loc (getByTestIdSelector testId)

getByRole :: Locator -> AriaRole -> GetByRoleOptions -> Locator
getByRole loc role opts =
  childLocator loc (getByRoleSelector role opts)

click :: Locator -> IO ()
click loc =
  clickFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

dblclick :: Locator -> IO ()
dblclick loc =
  dblclickFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

dispatchEvent :: Locator -> DomEventType -> IO ()
dispatchEvent loc eventType =
  dispatchEventFrame (locatorFrame loc) (locatorSelector loc) eventType defaultTimeout

elementHandle :: Locator -> IO (Maybe ElementHandle)
elementHandle loc = do
  waitFor loc WaitForAttached
  querySelectorFrame (locatorFrame loc) (locatorSelector loc) (Just True)

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

selectOption :: Locator -> [SelectOption] -> IO [Text]
selectOption loc options =
  selectOptionFrame (locatorFrame loc) (locatorSelector loc) options defaultTimeout

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

waitFor :: Locator -> WaitForState -> IO ()
waitFor loc =
  waitForWith loc defaultTimeout

waitForWith :: Locator -> Timeout -> WaitForState -> IO ()
waitForWith loc timeout state =
  waitForSelectorFrame (locatorFrame loc) (locatorSelector loc) state timeout (Just True)

module Playwright.Locator
  ( Locator (..)
  , allLocators
  , blur
  , chainLocator
  , check
  , checkWith
  , childLocator
  , childFrameLocator
  , clear
  , click
  , describeLocator
  , dblclick
  , dispatchEvent
  , dragAndDrop
  , elementHandle
  , elementHandles
  , fill
  , filterVisible
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
  , innerTextWith
  , inputValue
  , isChecked
  , isDisabled
  , isEditable
  , isEnabled
  , isHidden
  , isVisible
  , lastLocator
  , locatorBoundingBox
  , locatorCount
  , locatorDescription
  , locatorScrollIntoView
  , locatorSelectText
  , nth
  , press
  , pressSequentially
  , selectOption
  , setChecked
  , setInputFiles
  , tap
  , textContent
  , typeText
  , uncheck
  , uncheckWith
  , waitFor
  , waitForWith
  ) where

import Control.Exception
  ( finally
  , throwIO
  )
import Data.Aeson
  ( eitherDecodeStrict'
  , encode
  )
import Data.ByteString.Lazy
  ( toStrict
  )
import Data.Text
  ( Text
  , pack
  )
import Data.Text qualified as Text
import Data.Text.Encoding
  ( decodeUtf8
  , encodeUtf8
  )
import Playwright.AriaRole
  ( AriaRole
  )
import Playwright.BoundingBox
  ( BoundingBox
  )
import Playwright.Channel
  ( Channel
  , BrowserContextTag
  , PageTag
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
  , boundingBox
  , scrollIntoViewIfNeeded
  , selectText
  )
import Playwright.ElementHandle.Action
  ( disposeHandle
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Exactness
  ( Exactness
  )
import Playwright.Frame
  ( Frame
  )
import Playwright.Frame.Action
  ( blurFrame
  , checkFrame
  , clickFrame
  , dblclickFrame
  , dispatchEventFrame
  , dragAndDropFrame
  , fillFrame
  , setInputFilesFrame
  , focusFrame
  , hoverFrame
  , pressFrame
  , selectOptionFrame
  , tapFrame
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
  , querySelectorAllFrame
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
import Playwright.InputFile
  ( InputFile
  )
import Playwright.Key
  ( Key
  )
import Playwright.SelectOption
  ( SelectOption
  )
import Playwright.Selector
  ( Selector
  , enterFrameSelector
  , nestedSelector
  , selectorFromText
  , selectorToText
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
  , locatorPageChannel :: Channel PageTag
  , locatorContextChannel :: Channel BrowserContextTag
  , locatorMainFrame :: Frame
  }

childLocator :: Locator -> Selector -> Locator
childLocator parent child =
  Locator
    { locatorFrame = locatorFrame parent
    , locatorSelector = nestedSelector (locatorSelector parent) child
    , locatorPageChannel = locatorPageChannel parent
    , locatorContextChannel = locatorContextChannel parent
    , locatorMainFrame = locatorMainFrame parent
    }

chainLocator :: Locator -> Locator -> Locator
chainLocator parent inner =
  childLocator parent (selectorFromText ("internal:chain=" <> jsonEncodeText (selectorToText (locatorSelector inner))))

filterVisible :: Locator -> Bool -> Locator
filterVisible loc visible =
  childLocator loc (selectorFromText ("visible=" <> if visible then "true" else "false"))

describeLocator :: Locator -> Text -> Locator
describeLocator loc description =
  childLocator loc (selectorFromText ("internal:describe=" <> jsonEncodeText description))

locatorDescription :: Locator -> Maybe Text
locatorDescription loc =
  case Text.breakOnEnd "internal:describe=" (selectorToText (locatorSelector loc)) of
    (_, payload)
      | Text.null payload ->
          Nothing
      | otherwise ->
          case eitherDecodeStrict' (encodeUtf8 payload) of
            Right text ->
              Just text
            Left _ ->
              Nothing

jsonEncodeText :: Text -> Text
jsonEncodeText =
  decodeUtf8 . toStrict . encode

childFrameLocator :: Locator -> Selector -> Locator
childFrameLocator loc iframeSelector =
  loc
    { locatorSelector =
        nestedSelector
          (nestedSelector (locatorSelector loc) iframeSelector)
          enterFrameSelector
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

elementHandles :: Locator -> IO [ElementHandle]
elementHandles loc =
  querySelectorAllFrame (locatorFrame loc) (locatorSelector loc)

allLocators :: Locator -> IO [Locator]
allLocators loc = do
  n <- locatorCount loc
  pure [nth loc i | i <- [0 .. n - 1]]

fill :: Locator -> Text -> IO ()
fill loc value =
  fillFrame (locatorFrame loc) (locatorSelector loc) value defaultTimeout

clear :: Locator -> IO ()
clear loc =
  fill loc ""

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

blur :: Locator -> IO ()
blur loc =
  blurFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

hover :: Locator -> IO ()
hover loc =
  hoverFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

tap :: Locator -> IO ()
tap loc =
  tapFrame (locatorFrame loc) (locatorSelector loc) defaultTimeout

press :: Locator -> Key -> IO ()
press loc key =
  pressFrame (locatorFrame loc) (locatorSelector loc) key defaultTimeout

typeText :: Locator -> Text -> IO ()
typeText loc text =
  typeTextFrame (locatorFrame loc) (locatorSelector loc) text defaultTimeout

pressSequentially :: Locator -> Text -> IO ()
pressSequentially =
  typeText

setInputFiles :: Locator -> [InputFile] -> IO ()
setInputFiles loc files =
  setInputFilesFrame (locatorFrame loc) (locatorSelector loc) files defaultTimeout

dragAndDrop :: Locator -> Locator -> IO ()
dragAndDrop source target =
  dragAndDropFrame
    (locatorFrame source)
    (locatorSelector source)
    (locatorSelector target)
    defaultTimeout

selectOption :: Locator -> [SelectOption] -> IO [Text]
selectOption loc options =
  selectOptionFrame (locatorFrame loc) (locatorSelector loc) options defaultTimeout

innerText :: Locator -> IO Text
innerText loc =
  innerTextWith loc defaultTimeout

innerTextWith :: Locator -> Timeout -> IO Text
innerTextWith loc timeout =
  innerTextFrame (locatorFrame loc) (locatorSelector loc) timeout

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

withLocatorElement :: Locator -> (ElementHandle -> IO a) -> IO a
withLocatorElement loc action = do
  handle <- requireLocatorHandle loc
  action handle `finally` disposeHandle handle

requireLocatorHandle :: Locator -> IO ElementHandle
requireLocatorHandle loc = do
  found <- elementHandle loc
  case found of
    Just handle ->
      pure handle
    Nothing ->
      throwIO (DecodeFailure "Could not resolve locator to DOM Element")

locatorBoundingBox :: Locator -> IO (Maybe BoundingBox)
locatorBoundingBox loc =
  withLocatorElement loc boundingBox

locatorScrollIntoView :: Locator -> IO ()
locatorScrollIntoView loc =
  withLocatorElement loc scrollIntoViewIfNeeded

locatorSelectText :: Locator -> IO ()
locatorSelectText loc =
  withLocatorElement loc selectText

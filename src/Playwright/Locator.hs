module Playwright.Locator
  ( Locator (..)
  , allLocators
  , andLocator
  , blur
  , chainLocator
  , check
  , checkWith
  , childLocator
  , childFrameLocator
  , clear
  , click
  , clickWith
  , describeLocator
  , dblclick
  , dispatchEvent
  , dispatchEventWith
  , dragAndDrop
  , dropOnto
  , elementHandle
  , elementHandles
  , fill
  , filterHas
  , filterHasNot
  , filterHasNotText
  , filterHasText
  , filterHasTextRegex
  , filterHasTextWith
  , filterVisible
  , first
  , focus
  , getAttribute
  , getByLabel
  , getByRole
  , getByTestId
  , getByText
  , hover
  , highlight
  , hideHighlight
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
  , orLocator
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

import Playwright.Monad
  ( Playwright
  )
import UnliftIO.Exception
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
  ( Channel (channelGuid)
  , BrowserContextTag
  , PageTag
  )
import Playwright.CheckOptions
  ( CheckOptions (checkOptionsTimeout)
  , defaultCheckOptions
  )
import Playwright.ClickOptions
  ( ClickOptions (clickOptionsTimeout)
  )
import Playwright.DomEventType
  ( DomEventType
  )
import Playwright.SerializedArgument
  ( SerializedArgument
  )
import Playwright.DropPayload
  ( DropPayload
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
  ( Exactness (Inexact)
  )
import Playwright.Frame
  ( Frame
  )
import Playwright.Frame.Action
  ( blurFrame
  , checkFrame
  , clickFrame
  , clickFrameWith
  , dblclickFrame
  , dispatchEventFrame
  , dispatchEventFrameWith
  , dragAndDropFrame
  , dropFrame
  , fillFrame
  , setInputFilesFrame
  , focusFrame
  , hoverFrame
  , highlightFrame
  , hideHighlightFrame
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
  , hasNotTextSelector
  , hasTextRegexSelector
  , hasTextSelector
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
import Playwright.Connection
  ( lookupTimeoutDefaults
  )
import Playwright.Timeout
  ( Timeout
  )
import Playwright.TimeoutDefaults
  ( resolveActionTimeout
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
  childLocator parent (innerJsonSelector "internal:chain=" inner)

filterHas :: Locator -> Locator -> Locator
filterHas loc inner =
  childLocator loc (innerJsonSelector "internal:has=" inner)

filterHasNot :: Locator -> Locator -> Locator
filterHasNot loc inner =
  childLocator loc (innerJsonSelector "internal:has-not=" inner)

andLocator :: Locator -> Locator -> Locator
andLocator loc inner =
  childLocator loc (innerJsonSelector "internal:and=" inner)

orLocator :: Locator -> Locator -> Locator
orLocator loc inner =
  childLocator loc (innerJsonSelector "internal:or=" inner)

innerJsonSelector :: Text -> Locator -> Selector
innerJsonSelector prefix inner =
  selectorFromText (prefix <> jsonEncodeText (selectorToText (locatorSelector inner)))

filterVisible :: Locator -> Bool -> Locator
filterVisible loc visible =
  childLocator loc (selectorFromText ("visible=" <> if visible then "true" else "false"))

filterHasText :: Locator -> Text -> Locator
filterHasText loc text =
  filterHasTextWith loc text Inexact

filterHasTextWith :: Locator -> Text -> Exactness -> Locator
filterHasTextWith loc text exactness =
  childLocator loc (hasTextSelector text exactness)

filterHasNotText :: Locator -> Text -> Locator
filterHasNotText loc text =
  childLocator loc (hasNotTextSelector text Inexact)

filterHasTextRegex :: Locator -> Text -> Locator
filterHasTextRegex loc source =
  childLocator loc (hasTextRegexSelector source)

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

locatorActionTimeout :: Locator -> Playwright Timeout
locatorActionTimeout loc = do
  pageDefaults <- lookupTimeoutDefaults (channelGuid (locatorPageChannel loc))
  contextDefaults <- lookupTimeoutDefaults (channelGuid (locatorContextChannel loc))
  pure (resolveActionTimeout pageDefaults contextDefaults)

withActionTimeout :: Locator -> (Timeout -> Playwright a) -> Playwright a
withActionTimeout loc action = do
  timeout <- locatorActionTimeout loc
  action timeout

click :: Locator -> Playwright ()
click loc =
  withActionTimeout loc (clickFrame (locatorFrame loc) (locatorSelector loc))

clickWith :: Locator -> ClickOptions -> Playwright ()
clickWith loc opts = do
  timeout <-
    case clickOptionsTimeout opts of
      Just value ->
        pure value
      Nothing ->
        locatorActionTimeout loc
  clickFrameWith
    (locatorFrame loc)
    (locatorSelector loc)
    timeout
    opts

dblclick :: Locator -> Playwright ()
dblclick loc =
  withActionTimeout loc (dblclickFrame (locatorFrame loc) (locatorSelector loc))

dispatchEvent :: Locator -> DomEventType -> Playwright ()
dispatchEvent loc eventType =
  withActionTimeout loc (dispatchEventFrame (locatorFrame loc) (locatorSelector loc) eventType)

dispatchEventWith :: Locator -> DomEventType -> SerializedArgument -> Playwright ()
dispatchEventWith loc eventType eventInit =
  withActionTimeout loc $ \timeout ->
    dispatchEventFrameWith
      (locatorFrame loc)
      (locatorSelector loc)
      eventType
      eventInit
      timeout

fill :: Locator -> Text -> Playwright ()
fill loc value =
  withActionTimeout loc (\timeout -> fillFrame (locatorFrame loc) (locatorSelector loc) value timeout)

elementHandle :: Locator -> Playwright (Maybe ElementHandle)
elementHandle loc = do
  waitFor loc WaitForAttached
  querySelectorFrame (locatorFrame loc) (locatorSelector loc) (Just True)

elementHandles :: Locator -> Playwright [ElementHandle]
elementHandles loc =
  querySelectorAllFrame (locatorFrame loc) (locatorSelector loc)

allLocators :: Locator -> Playwright [Locator]
allLocators loc = do
  n <- locatorCount loc
  pure [nth loc i | i <- [0 .. n - 1]]

clear :: Locator -> Playwright ()
clear loc =
  fill loc ""

check :: Locator -> Playwright ()
check loc = do
  timeout <- locatorActionTimeout loc
  checkWith loc defaultCheckOptions {checkOptionsTimeout = timeout}

checkWith :: Locator -> CheckOptions -> Playwright ()
checkWith loc =
  checkFrame (locatorFrame loc) (locatorSelector loc)

uncheck :: Locator -> Playwright ()
uncheck loc = do
  timeout <- locatorActionTimeout loc
  uncheckWith loc defaultCheckOptions {checkOptionsTimeout = timeout}

uncheckWith :: Locator -> CheckOptions -> Playwright ()
uncheckWith loc =
  uncheckFrame (locatorFrame loc) (locatorSelector loc)

setChecked :: Locator -> Bool -> Playwright ()
setChecked loc wanted =
  if wanted
    then check loc
    else uncheck loc

focus :: Locator -> Playwright ()
focus loc =
  withActionTimeout loc (focusFrame (locatorFrame loc) (locatorSelector loc))

blur :: Locator -> Playwright ()
blur loc =
  withActionTimeout loc (blurFrame (locatorFrame loc) (locatorSelector loc))

hover :: Locator -> Playwright ()
hover loc =
  withActionTimeout loc (hoverFrame (locatorFrame loc) (locatorSelector loc))

highlight :: Locator -> Playwright ()
highlight loc =
  highlightFrame (locatorFrame loc) (locatorSelector loc)

hideHighlight :: Locator -> Playwright ()
hideHighlight loc =
  hideHighlightFrame (locatorFrame loc) (locatorSelector loc)

tap :: Locator -> Playwright ()
tap loc =
  withActionTimeout loc (tapFrame (locatorFrame loc) (locatorSelector loc))

press :: Locator -> Key -> Playwright ()
press loc key =
  withActionTimeout loc (pressFrame (locatorFrame loc) (locatorSelector loc) key)

typeText :: Locator -> Text -> Playwright ()
typeText loc text =
  withActionTimeout loc (typeTextFrame (locatorFrame loc) (locatorSelector loc) text)

pressSequentially :: Locator -> Text -> Playwright ()
pressSequentially =
  typeText

setInputFiles :: Locator -> [InputFile] -> Playwright ()
setInputFiles loc files =
  withActionTimeout loc (setInputFilesFrame (locatorFrame loc) (locatorSelector loc) files)

dragAndDrop :: Locator -> Locator -> Playwright ()
dragAndDrop source target =
  withActionTimeout
    source
    ( dragAndDropFrame
        (locatorFrame source)
        (locatorSelector source)
        (locatorSelector target)
    )

dropOnto :: Locator -> DropPayload -> Playwright ()
dropOnto loc payload =
  withActionTimeout loc (dropFrame (locatorFrame loc) (locatorSelector loc) payload)

selectOption :: Locator -> [SelectOption] -> Playwright [Text]
selectOption loc options =
  withActionTimeout loc (selectOptionFrame (locatorFrame loc) (locatorSelector loc) options)

innerText :: Locator -> Playwright Text
innerText loc =
  withActionTimeout loc (innerTextWith loc)

innerTextWith :: Locator -> Timeout -> Playwright Text
innerTextWith loc timeout =
  innerTextFrame (locatorFrame loc) (locatorSelector loc) timeout

innerHtml :: Locator -> Playwright Text
innerHtml loc =
  withActionTimeout loc (innerHtmlFrame (locatorFrame loc) (locatorSelector loc))

textContent :: Locator -> Playwright (Maybe Text)
textContent loc =
  withActionTimeout loc (textContentFrame (locatorFrame loc) (locatorSelector loc))

getAttribute :: Locator -> Text -> Playwright (Maybe Text)
getAttribute loc name =
  withActionTimeout loc (getAttributeFrame (locatorFrame loc) (locatorSelector loc) name)

inputValue :: Locator -> Playwright Text
inputValue loc =
  withActionTimeout loc (inputValueFrame (locatorFrame loc) (locatorSelector loc))

isVisible :: Locator -> Playwright Bool
isVisible loc =
  isVisibleFrame (locatorFrame loc) (locatorSelector loc)

isHidden :: Locator -> Playwright Bool
isHidden loc =
  isHiddenFrame (locatorFrame loc) (locatorSelector loc)

isChecked :: Locator -> Playwright Bool
isChecked loc =
  withActionTimeout loc (isCheckedFrame (locatorFrame loc) (locatorSelector loc))

isEnabled :: Locator -> Playwright Bool
isEnabled loc =
  withActionTimeout loc (isEnabledFrame (locatorFrame loc) (locatorSelector loc))

isDisabled :: Locator -> Playwright Bool
isDisabled loc =
  withActionTimeout loc (isDisabledFrame (locatorFrame loc) (locatorSelector loc))

isEditable :: Locator -> Playwright Bool
isEditable loc =
  withActionTimeout loc (isEditableFrame (locatorFrame loc) (locatorSelector loc))

locatorCount :: Locator -> Playwright Int
locatorCount loc =
  queryCountFrame (locatorFrame loc) (locatorSelector loc)

waitFor :: Locator -> WaitForState -> Playwright ()
waitFor loc state = do
  timeout <- locatorActionTimeout loc
  waitForWith loc timeout state

waitForWith :: Locator -> Timeout -> WaitForState -> Playwright ()
waitForWith loc timeout state =
  waitForSelectorFrame (locatorFrame loc) (locatorSelector loc) state timeout (Just True)

withLocatorElement :: Locator -> (ElementHandle -> Playwright a) -> Playwright a
withLocatorElement loc action = do
  handle <- requireLocatorHandle loc
  action handle `finally` disposeHandle handle

requireLocatorHandle :: Locator -> Playwright ElementHandle
requireLocatorHandle loc = do
  found <- elementHandle loc
  case found of
    Just handle ->
      pure handle
    Nothing ->
      throwIO (DecodeFailure "Could not resolve locator to DOM Element")

locatorBoundingBox :: Locator -> Playwright (Maybe BoundingBox)
locatorBoundingBox loc =
  withLocatorElement loc boundingBox

locatorScrollIntoView :: Locator -> Playwright ()
locatorScrollIntoView loc =
  withLocatorElement loc scrollIntoViewIfNeeded

locatorSelectText :: Locator -> Playwright ()
locatorSelectText loc =
  withLocatorElement loc selectText

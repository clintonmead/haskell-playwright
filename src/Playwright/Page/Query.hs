module Playwright.Page.Query
  ( pageGetAttribute
  , pageInnerHtml
  , pageInnerText
  , pageInputValue
  , pageIsChecked
  , pageIsDisabled
  , pageIsEditable
  , pageIsEnabled
  , pageIsHidden
  , pageIsVisible
  , pageTextContent
  , pageTextContentWith
  , pageClickSelector
  ) where

import Playwright.Monad
  ( Playwright
  )
import Data.Text
  ( Text
  )
import Playwright.Frame.Action
  ( clickFrameStrict
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
  , textContentFrameStrict
  )
import Playwright.Page
  ( Page (pageMainFrame)
  , pageActionTimeout
  )
import Playwright.Selector
  ( Selector
  )

pageInnerText :: Page -> Selector -> Playwright Text
pageInnerText page selector = do
  timeout <- pageActionTimeout page
  innerTextFrame (pageMainFrame page) selector timeout

pageInnerHtml :: Page -> Selector -> Playwright Text
pageInnerHtml page selector = do
  timeout <- pageActionTimeout page
  innerHtmlFrame (pageMainFrame page) selector timeout

pageTextContent :: Page -> Selector -> Playwright (Maybe Text)
pageTextContent page selector =
  pageTextContentWith page selector Nothing

pageTextContentWith :: Page -> Selector -> Maybe Bool -> Playwright (Maybe Text)
pageTextContentWith page selector strict = do
  timeout <- pageActionTimeout page
  textContentFrameStrict (pageMainFrame page) selector timeout strict

pageClickSelector :: Page -> Selector -> Playwright ()
pageClickSelector page selector = do
  timeout <- pageActionTimeout page
  clickFrameStrict (pageMainFrame page) selector timeout Nothing

pageGetAttribute :: Page -> Selector -> Text -> Playwright (Maybe Text)
pageGetAttribute page selector name = do
  timeout <- pageActionTimeout page
  getAttributeFrame (pageMainFrame page) selector name timeout

pageInputValue :: Page -> Selector -> Playwright Text
pageInputValue page selector = do
  timeout <- pageActionTimeout page
  inputValueFrame (pageMainFrame page) selector timeout

pageIsChecked :: Page -> Selector -> Playwright Bool
pageIsChecked page selector = do
  timeout <- pageActionTimeout page
  isCheckedFrame (pageMainFrame page) selector timeout

pageIsEnabled :: Page -> Selector -> Playwright Bool
pageIsEnabled page selector = do
  timeout <- pageActionTimeout page
  isEnabledFrame (pageMainFrame page) selector timeout

pageIsDisabled :: Page -> Selector -> Playwright Bool
pageIsDisabled page selector = do
  timeout <- pageActionTimeout page
  isDisabledFrame (pageMainFrame page) selector timeout

pageIsEditable :: Page -> Selector -> Playwright Bool
pageIsEditable page selector = do
  timeout <- pageActionTimeout page
  isEditableFrame (pageMainFrame page) selector timeout

pageIsVisible :: Page -> Selector -> Playwright Bool
pageIsVisible page selector =
  isVisibleFrame (pageMainFrame page) selector

pageIsHidden :: Page -> Selector -> Playwright Bool
pageIsHidden page selector =
  isHiddenFrame (pageMainFrame page) selector

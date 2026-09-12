module Playwright.Page.Query
  ( pageGetAttribute
  , pageInnerHtml
  , pageInnerText
  , pageInputValue
  , pageIsChecked
  , pageIsDisabled
  , pageIsEditable
  , pageIsEnabled
  , pageIsVisible
  , pageTextContent
  ) where

import Data.Text
  ( Text
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
  , isVisibleFrame
  , textContentFrame
  )
import Playwright.Page
  ( Page (pageMainFrame)
  , pageActionTimeout
  )
import Playwright.Selector
  ( Selector
  )

pageInnerText :: Page -> Selector -> IO Text
pageInnerText page selector = do
  timeout <- pageActionTimeout page
  innerTextFrame (pageMainFrame page) selector timeout

pageInnerHtml :: Page -> Selector -> IO Text
pageInnerHtml page selector = do
  timeout <- pageActionTimeout page
  innerHtmlFrame (pageMainFrame page) selector timeout

pageTextContent :: Page -> Selector -> IO (Maybe Text)
pageTextContent page selector = do
  timeout <- pageActionTimeout page
  textContentFrame (pageMainFrame page) selector timeout

pageGetAttribute :: Page -> Selector -> Text -> IO (Maybe Text)
pageGetAttribute page selector name = do
  timeout <- pageActionTimeout page
  getAttributeFrame (pageMainFrame page) selector name timeout

pageInputValue :: Page -> Selector -> IO Text
pageInputValue page selector = do
  timeout <- pageActionTimeout page
  inputValueFrame (pageMainFrame page) selector timeout

pageIsChecked :: Page -> Selector -> IO Bool
pageIsChecked page selector = do
  timeout <- pageActionTimeout page
  isCheckedFrame (pageMainFrame page) selector timeout

pageIsEnabled :: Page -> Selector -> IO Bool
pageIsEnabled page selector = do
  timeout <- pageActionTimeout page
  isEnabledFrame (pageMainFrame page) selector timeout

pageIsDisabled :: Page -> Selector -> IO Bool
pageIsDisabled page selector = do
  timeout <- pageActionTimeout page
  isDisabledFrame (pageMainFrame page) selector timeout

pageIsEditable :: Page -> Selector -> IO Bool
pageIsEditable page selector = do
  timeout <- pageActionTimeout page
  isEditableFrame (pageMainFrame page) selector timeout

pageIsVisible :: Page -> Selector -> IO Bool
pageIsVisible page selector =
  isVisibleFrame (pageMainFrame page) selector

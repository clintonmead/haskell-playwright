module Playwright.Page.Query
  ( pageGetAttribute
  , pageInnerHtml
  , pageInnerText
  , pageInputValue
  , pageIsChecked
  , pageIsDisabled
  , pageIsEditable
  , pageIsEnabled
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
  , textContentFrame
  )
import Playwright.Page
  ( Page (pageMainFrame)
  )
import Playwright.Selector
  ( Selector
  )
import Playwright.Timeout
  ( defaultTimeout
  )

pageInnerText :: Page -> Selector -> IO Text
pageInnerText page selector =
  innerTextFrame (pageMainFrame page) selector defaultTimeout

pageInnerHtml :: Page -> Selector -> IO Text
pageInnerHtml page selector =
  innerHtmlFrame (pageMainFrame page) selector defaultTimeout

pageTextContent :: Page -> Selector -> IO (Maybe Text)
pageTextContent page selector =
  textContentFrame (pageMainFrame page) selector defaultTimeout

pageGetAttribute :: Page -> Selector -> Text -> IO (Maybe Text)
pageGetAttribute page selector name =
  getAttributeFrame (pageMainFrame page) selector name defaultTimeout

pageInputValue :: Page -> Selector -> IO Text
pageInputValue page selector =
  inputValueFrame (pageMainFrame page) selector defaultTimeout

pageIsChecked :: Page -> Selector -> IO Bool
pageIsChecked page selector =
  isCheckedFrame (pageMainFrame page) selector defaultTimeout

pageIsEnabled :: Page -> Selector -> IO Bool
pageIsEnabled page selector =
  isEnabledFrame (pageMainFrame page) selector defaultTimeout

pageIsDisabled :: Page -> Selector -> IO Bool
pageIsDisabled page selector =
  isDisabledFrame (pageMainFrame page) selector defaultTimeout

pageIsEditable :: Page -> Selector -> IO Bool
pageIsEditable page selector =
  isEditableFrame (pageMainFrame page) selector defaultTimeout

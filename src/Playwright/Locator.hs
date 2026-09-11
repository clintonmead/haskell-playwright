module Playwright.Locator
  ( Locator (..)
  , childLocator
  , click
  , fill
  , innerText
  , innerHtml
  , textContent
  , getAttribute
  , inputValue
  , isVisible
  , isHidden
  ) where

import Data.Text
  ( Text
  )
import Playwright.Frame
  ( Frame
  , clickFrame
  , fillFrame
  , getAttributeFrame
  , innerHtmlFrame
  , innerTextFrame
  , inputValueFrame
  , isHiddenFrame
  , isVisibleFrame
  , textContentFrame
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

fill :: Locator -> Text -> IO ()
fill loc value =
  fillFrame (locatorFrame loc) (locatorSelector loc) value defaultTimeout

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

module Playwright.HighlightStyle
  ( cssStyleText
  , highlightWithStyleProperties
  ) where

import Data.Char
  ( isUpper
  , toLower
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Playwright.Frame.Action
  ( highlightFrameWithStyle
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.Monad
  ( Playwright
  )

-- | Highlights the element like 'Playwright.Locator.highlightWithStyle', but
-- takes the style as property/value pairs. camelCase property names become
-- kebab-case CSS properties, custom properties (@--name@) are kept as they
-- are, and the pairs are joined with @"; "@, matching the upstream client.
highlightWithStyleProperties :: Locator -> [(Text, Text)] -> Playwright ()
highlightWithStyleProperties loc props =
  highlightFrameWithStyle (locatorFrame loc) (locatorSelector loc) (cssStyleText props)

-- | Renders property/value pairs as CSS declaration text.
cssStyleText :: [(Text, Text)] -> Text
cssStyleText props =
  Text.intercalate "; " [cssProperty name <> ": " <> value | (name, value) <- props]
  where
    cssProperty name
      | Text.isPrefixOf "--" name = name
      | otherwise = Text.concatMap kebab name
    kebab c
      | isUpper c = Text.pack ['-', toLower c]
      | otherwise = Text.singleton c

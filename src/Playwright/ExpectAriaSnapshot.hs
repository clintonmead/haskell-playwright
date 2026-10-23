module Playwright.ExpectAriaSnapshot
  ( expectPageAriaSnapshot
  , expectLocatorAriaSnapshot
  , unshiftAriaTemplate
  ) where

import Data.Char
  ( isSpace
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as Text
import Playwright.Expect
  ( ExpectOptions (expectIsNot)
  , sendExpect
  )
import Playwright.Locator
  ( Locator (locatorFrame, locatorSelector)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page
  , mainFrame
  )
import Playwright.Protocol
  ( FrameExpectParams
  , frameExpectParamsExpectedValue
  , frameExpectParamsSelector
  , mkFrameExpectParams
  )
import Playwright.SerializedArgument
  ( textArgument
  )

-- | @toMatchAriaSnapshot@ on a page. The template is unshifted first and
-- matched by the driver against the page body.
expectPageAriaSnapshot :: ExpectOptions -> Page -> Text -> Playwright ()
expectPageAriaSnapshot options page template =
  sendExpect
    options
    (mainFrame page)
    (mkAriaParams options template)

-- | @toMatchAriaSnapshot@ on a locator. The template is unshifted first and
-- matched by the driver against the locator's element.
expectLocatorAriaSnapshot :: ExpectOptions -> Locator -> Text -> Playwright ()
expectLocatorAriaSnapshot options locator template =
  sendExpect
    options
    (locatorFrame locator)
    (mkAriaParams options template)
      { frameExpectParamsSelector = Just (locatorSelector locator)
      }

mkAriaParams :: ExpectOptions -> Text -> FrameExpectParams
mkAriaParams options template =
  (mkFrameExpectParams "to.match.aria" (expectIsNot options))
    { frameExpectParamsExpectedValue = Just (textArgument (unshiftAriaTemplate template))
    }

-- | Strip blank lines and the common leading indentation, as upstream does
-- before sending a template to the driver.
unshiftAriaTemplate :: Text -> Text
unshiftAriaTemplate snapshot =
  Text.intercalate "\n" (map (Text.drop prefixLength) nonBlank)
  where
    nonBlank :: [Text]
    nonBlank = filter (not . Text.all isSpace) (Text.lines snapshot)

    prefixLength :: Int
    prefixLength = minimum (100 : map (Text.length . Text.takeWhile isSpace) nonBlank)

module Playwright.ClearCookiesOptions
  ( ClearCookiesOptions (..)
  , defaultClearCookiesOptions
  ) where

import Data.Text
  ( Text
  )
import Playwright.Cookie
  ( CookieDomain
  , CookieName
  , CookiePath
  )

data ClearCookiesOptions = ClearCookiesOptions
  { clearCookiesName :: Maybe CookieName
  , clearCookiesNameRegex :: Maybe Text
  , clearCookiesDomain :: Maybe CookieDomain
  , clearCookiesPath :: Maybe CookiePath
  }
  deriving stock (Eq, Show)

defaultClearCookiesOptions :: ClearCookiesOptions
defaultClearCookiesOptions =
  ClearCookiesOptions
    { clearCookiesName = Nothing
    , clearCookiesNameRegex = Nothing
    , clearCookiesDomain = Nothing
    , clearCookiesPath = Nothing
    }

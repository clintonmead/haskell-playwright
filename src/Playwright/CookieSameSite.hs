module Playwright.CookieSameSite
  ( CookieSameSite (..)
  ) where

data CookieSameSite
  = CookieSameSiteStrict
  | CookieSameSiteLax
  | CookieSameSiteNone
  deriving stock (Eq, Ord, Show)

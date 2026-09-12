module Playwright.AcceptDownloads
  ( AcceptDownloads (..)
  ) where

data AcceptDownloads
  = AcceptDownloadsAccept
  | AcceptDownloadsDeny
  deriving stock (Eq, Show)

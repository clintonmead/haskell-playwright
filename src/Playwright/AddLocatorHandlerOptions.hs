module Playwright.AddLocatorHandlerOptions
  ( AddLocatorHandlerOptions (..)
  , LocatorHandlerTimes (..)
  , defaultAddLocatorHandlerOptions
  ) where

data LocatorHandlerTimes = LocatorHandlerTimes
  { locatorHandlerTimesToInt :: Int
  }
  deriving stock (Eq, Show)

data AddLocatorHandlerOptions = AddLocatorHandlerOptions
  { addLocatorHandlerTimes :: Maybe LocatorHandlerTimes
  , addLocatorHandlerNoWaitAfter :: Maybe Bool
  }
  deriving stock (Eq, Show)

defaultAddLocatorHandlerOptions :: AddLocatorHandlerOptions
defaultAddLocatorHandlerOptions =
  AddLocatorHandlerOptions
    { addLocatorHandlerTimes = Nothing
    , addLocatorHandlerNoWaitAfter = Nothing
    }

module Playwright.RouteOptions
  ( RouteOptions (..)
  , RouteTimes (..)
  , defaultRouteOptions
  ) where

data RouteTimes = RouteTimes
  { routeTimesToInt :: Int
  }
  deriving stock (Eq, Show)

data RouteOptions = RouteOptions
  { routeTimes :: Maybe RouteTimes
  }
  deriving stock (Eq, Show)

defaultRouteOptions :: RouteOptions
defaultRouteOptions =
  RouteOptions
    { routeTimes = Nothing
    }

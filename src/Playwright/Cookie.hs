module Playwright.Cookie
  ( Cookie (..)
  , CookieDomain (..)
  , CookieExpires (..)
  , CookieHttpOnly (..)
  , CookieName (..)
  , CookiePath (..)
  , CookieSecure (..)
  , CookieToSet (..)
  , CookieValue (..)
  , cookieForUrl
  , fromNetworkCookie
  , sessionExpires
  , toNetworkCookie
  , toSetNetworkCookie
  , toSetNetworkFromCookie
  ) where

import Data.Scientific
  ( Scientific
  )
import Data.Text
  ( Text
  )
import Playwright.CookieSameSite
  ( CookieSameSite (CookieSameSiteLax, CookieSameSiteNone, CookieSameSiteStrict)
  )
import Playwright.Protocol
  ( NetworkCookie (networkCookieDomain, networkCookieExpires, networkCookieHttpOnly, networkCookieName, networkCookiePath, networkCookieSameSite, networkCookieSecure, networkCookieValue)
  , NetworkCookieSameSite (NetworkCookieSameSiteLax, NetworkCookieSameSiteNone, NetworkCookieSameSiteStrict)
  , SetNetworkCookie (setNetworkCookieDomain, setNetworkCookieExpires, setNetworkCookieHttpOnly, setNetworkCookiePath, setNetworkCookieSameSite, setNetworkCookieSecure, setNetworkCookieUrl)
  , SetNetworkCookieSameSite (SetNetworkCookieSameSiteLax, SetNetworkCookieSameSiteNone, SetNetworkCookieSameSiteStrict)
  , mkNetworkCookie
  , mkSetNetworkCookie
  )
import Playwright.Url
  ( Url
  )

newtype CookieName = CookieName
  { cookieNameToText :: Text
  }
  deriving stock (Eq, Show)

newtype CookieValue = CookieValue
  { cookieValueToText :: Text
  }
  deriving stock (Eq, Show)

newtype CookieDomain = CookieDomain
  { cookieDomainToText :: Text
  }
  deriving stock (Eq, Show)

newtype CookiePath = CookiePath
  { cookiePathToText :: Text
  }
  deriving stock (Eq, Show)

newtype CookieExpires = CookieExpires
  { cookieExpiresToScientific :: Scientific
  }
  deriving stock (Eq, Ord, Show)

newtype CookieHttpOnly = CookieHttpOnly
  { cookieHttpOnlyToBool :: Bool
  }
  deriving stock (Eq, Show)

newtype CookieSecure = CookieSecure
  { cookieSecureToBool :: Bool
  }
  deriving stock (Eq, Show)

data Cookie = Cookie
  { cookieName :: CookieName
  , cookieValue :: CookieValue
  , cookieDomain :: CookieDomain
  , cookiePath :: CookiePath
  , cookieExpires :: CookieExpires
  , cookieHttpOnly :: CookieHttpOnly
  , cookieSecure :: CookieSecure
  , cookieSameSite :: CookieSameSite
  }
  deriving stock (Eq, Show)

data CookieToSet = CookieToSet
  { cookieToSetName :: CookieName
  , cookieToSetValue :: CookieValue
  , cookieToSetUrl :: Maybe Url
  , cookieToSetDomain :: Maybe CookieDomain
  , cookieToSetPath :: Maybe CookiePath
  , cookieToSetExpires :: Maybe CookieExpires
  , cookieToSetHttpOnly :: Maybe CookieHttpOnly
  , cookieToSetSecure :: Maybe CookieSecure
  , cookieToSetSameSite :: Maybe CookieSameSite
  }
  deriving stock (Eq, Show)

sessionExpires :: CookieExpires
sessionExpires =
  CookieExpires (fromInteger (-1) :: Scientific)

cookieForUrl :: Url -> CookieName -> CookieValue -> CookieToSet
cookieForUrl url name value =
  CookieToSet
    { cookieToSetName = name
    , cookieToSetValue = value
    , cookieToSetUrl = Just url
    , cookieToSetDomain = Nothing
    , cookieToSetPath = Nothing
    , cookieToSetExpires = Nothing
    , cookieToSetHttpOnly = Nothing
    , cookieToSetSecure = Nothing
    , cookieToSetSameSite = Nothing
    }

fromNetworkCookie :: NetworkCookie -> Cookie
fromNetworkCookie cookie =
  Cookie
    { cookieName = CookieName (networkCookieName cookie)
    , cookieValue = CookieValue (networkCookieValue cookie)
    , cookieDomain = CookieDomain (networkCookieDomain cookie)
    , cookiePath = CookiePath (networkCookiePath cookie)
    , cookieExpires = CookieExpires (networkCookieExpires cookie)
    , cookieHttpOnly = CookieHttpOnly (networkCookieHttpOnly cookie)
    , cookieSecure = CookieSecure (networkCookieSecure cookie)
    , cookieSameSite = fromNetworkSameSite (networkCookieSameSite cookie)
    }

fromNetworkSameSite :: NetworkCookieSameSite -> CookieSameSite
fromNetworkSameSite sameSite =
  case sameSite of
    NetworkCookieSameSiteStrict ->
      CookieSameSiteStrict
    NetworkCookieSameSiteLax ->
      CookieSameSiteLax
    NetworkCookieSameSiteNone ->
      CookieSameSiteNone

toSetNetworkCookie :: CookieToSet -> SetNetworkCookie
toSetNetworkCookie cookie =
  ( mkSetNetworkCookie
      (cookieNameToText (cookieToSetName cookie))
      (cookieValueToText (cookieToSetValue cookie))
  )
    { setNetworkCookieUrl = cookieToSetUrl cookie
    , setNetworkCookieDomain = fmap cookieDomainToText (cookieToSetDomain cookie)
    , setNetworkCookiePath = fmap cookiePathToText (cookieToSetPath cookie)
    , setNetworkCookieExpires = fmap cookieExpiresToScientific (cookieToSetExpires cookie)
    , setNetworkCookieHttpOnly = fmap cookieHttpOnlyToBool (cookieToSetHttpOnly cookie)
    , setNetworkCookieSecure = fmap cookieSecureToBool (cookieToSetSecure cookie)
    , setNetworkCookieSameSite = fmap toSetSameSite (cookieToSetSameSite cookie)
    }

toSetSameSite :: CookieSameSite -> SetNetworkCookieSameSite
toSetSameSite sameSite =
  case sameSite of
    CookieSameSiteStrict ->
      SetNetworkCookieSameSiteStrict
    CookieSameSiteLax ->
      SetNetworkCookieSameSiteLax
    CookieSameSiteNone ->
      SetNetworkCookieSameSiteNone

toSetNetworkFromCookie :: Cookie -> SetNetworkCookie
toSetNetworkFromCookie cookie =
  ( mkSetNetworkCookie
      (cookieNameToText (cookieName cookie))
      (cookieValueToText (cookieValue cookie))
  )
    { setNetworkCookieDomain = Just (cookieDomainToText (cookieDomain cookie))
    , setNetworkCookiePath = Just (cookiePathToText (cookiePath cookie))
    , setNetworkCookieExpires = Just (cookieExpiresToScientific (cookieExpires cookie))
    , setNetworkCookieHttpOnly = Just (cookieHttpOnlyToBool (cookieHttpOnly cookie))
    , setNetworkCookieSecure = Just (cookieSecureToBool (cookieSecure cookie))
    , setNetworkCookieSameSite = Just (toSetSameSite (cookieSameSite cookie))
    }

toNetworkCookie :: Cookie -> NetworkCookie
toNetworkCookie cookie =
  mkNetworkCookie
    (cookieDomainToText (cookieDomain cookie))
    (cookieExpiresToScientific (cookieExpires cookie))
    (cookieHttpOnlyToBool (cookieHttpOnly cookie))
    (cookieNameToText (cookieName cookie))
    (cookiePathToText (cookiePath cookie))
    (toNetworkSameSite (cookieSameSite cookie))
    (cookieSecureToBool (cookieSecure cookie))
    (cookieValueToText (cookieValue cookie))

toNetworkSameSite :: CookieSameSite -> NetworkCookieSameSite
toNetworkSameSite sameSite =
  case sameSite of
    CookieSameSiteStrict ->
      NetworkCookieSameSiteStrict
    CookieSameSiteLax ->
      NetworkCookieSameSiteLax
    CookieSameSiteNone ->
      NetworkCookieSameSiteNone

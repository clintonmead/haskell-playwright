module Playwright.Har
  ( HarNotFound (..)
  , HarRouteOptions (..)
  , defaultHarRouteOptions
  , routeFromHar
  , routeFromHarWith
  , contextRouteFromHar
  , contextRouteFromHarWith
  ) where

import Autodocodec
  ( Autodocodec (Autodocodec)
  , HasCodec (codec)
  , object
  , optionalField'
  , requiredField'
  , (.=)
  )
import Data.Aeson
  ( FromJSON
  )
import Data.Aeson qualified as Aeson
import Data.ByteString
  ( ByteString
  )
import Data.ByteString.Base64 qualified as Base64
import Data.Maybe
  ( fromMaybe
  )
import Data.Text
  ( Text
  )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Playwright.BrowserContext
  ( BrowserContext
  )
import Playwright.Connection
  ( lookupObject
  , lookupPlaywrightGuid
  , sendCommand
  )
import Playwright.Error
  ( PlaywrightError (ProtocolRpc)
  , ProtocolFailure
      ( ProtocolFailure
      , protocolFailureMessage
      , protocolFailureName
      , protocolFailureStack
      )
  )
import Playwright.FulfillOptions
  ( FulfillBody (FulfillBytes)
  , FulfillOptions (fulfillBody, fulfillHeaders, fulfillStatus)
  , defaultFulfillOptions
  )
import Playwright.Guid
  ( Guid
  )
import Playwright.HttpHeader
  ( HeaderName (HeaderName)
  , HeaderValue (HeaderValue)
  , HttpHeader (HttpHeader)
  , toNameValue
  )
import Playwright.HttpMethod
  ( HttpMethod (httpMethodToText)
  )
import Playwright.HttpStatus
  ( HttpStatus (HttpStatus)
  )
import Playwright.Json.ChannelRef
  ( ChannelRef (channelRefGuid)
  )
import Playwright.Json.Decode
  ( throwDecode
  )
import Playwright.MethodName
  ( MethodName (MethodName)
  )
import Playwright.Monad
  ( Playwright
  )
import Playwright.Page
  ( Page
  )
import Playwright.Protocol
  ( NameValue (nameValueName, nameValueValue)
  )
import Playwright.RemoteObject
  ( RemoteObject (remoteInitializer)
  )
import Playwright.Request
  ( requestAllHeaders
  , requestIsNavigation
  , requestMethod
  , requestPostData
  , requestUrl
  )
import Playwright.Route
  ( Route
  , abortRoute
  , fallbackRoute
  , fulfillRoute
  , redirectRoute
  , routeRequest
  )
import Playwright.Route.Intercept
  ( contextRoute
  , route
  )
import Playwright.UrlMatch
  ( UrlMatch (UrlMatchGlob)
  )
import Playwright.Url
  ( urlFromText
  , urlToText
  )
import UnliftIO.Exception
  ( throwIO
  )

-- | What to do with a request that the HAR file has no entry for.
data HarNotFound
  = HarNotFoundAbort
  | HarNotFoundFallback
  deriving stock (Eq, Show)

data HarRouteOptions = HarRouteOptions
  { harRouteUrl :: UrlMatch
  , harRouteNotFound :: HarNotFound
  }
  deriving stock (Eq, Show)

defaultHarRouteOptions :: HarRouteOptions
defaultHarRouteOptions =
  HarRouteOptions
    { harRouteUrl = UrlMatchGlob "**/*"
    , harRouteNotFound = HarNotFoundAbort
    }

-- | Serves matching requests from a HAR file, aborting unmatched ones.
routeFromHar :: Page -> FilePath -> Playwright ()
routeFromHar page file =
  routeFromHarWith page file defaultHarRouteOptions

routeFromHarWith :: Page -> FilePath -> HarRouteOptions -> Playwright ()
routeFromHarWith page file opts = do
  harId <- openHar file
  route page (harRouteUrl opts) (handleHarRoute harId opts)

contextRouteFromHar :: BrowserContext -> FilePath -> Playwright ()
contextRouteFromHar ctx file =
  contextRouteFromHarWith ctx file defaultHarRouteOptions

contextRouteFromHarWith :: BrowserContext -> FilePath -> HarRouteOptions -> Playwright ()
contextRouteFromHarWith ctx file opts = do
  harId <- openHar file
  contextRoute ctx (harRouteUrl opts) (handleHarRoute harId opts)

-- | The playwright object's initializer, which names the @utils@ channel.
newtype PlaywrightInitializer = PlaywrightInitializer
  { playwrightUtils :: Maybe ChannelRef
  }
  deriving stock (Eq, Show)
  deriving (FromJSON) via (Autodocodec PlaywrightInitializer)

instance HasCodec PlaywrightInitializer where
  codec =
    object "PlaywrightInitializer" $
      PlaywrightInitializer <$> optionalField' "utils" .= playwrightUtils

data HarOpenResult = HarOpenResult
  { harOpenId :: Maybe Text
  , harOpenError :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON) via (Autodocodec HarOpenResult)

instance HasCodec HarOpenResult where
  codec =
    object "HarOpenResult" $
      HarOpenResult
        <$> optionalField' "harId" .= harOpenId
        <*> optionalField' "error" .= harOpenError

data HarLookupResult = HarLookupResult
  { harLookupAction :: Text
  , harLookupRedirectUrl :: Maybe Text
  , harLookupStatus :: Maybe Int
  , harLookupHeaders :: Maybe [NameValue]
  , harLookupBody :: Maybe Text
  }
  deriving stock (Eq, Show)
  deriving (FromJSON) via (Autodocodec HarLookupResult)

instance HasCodec HarLookupResult where
  codec =
    object "HarLookupResult" $
      HarLookupResult
        <$> requiredField' "action" .= harLookupAction
        <*> optionalField' "redirectURL" .= harLookupRedirectUrl
        <*> optionalField' "status" .= harLookupStatus
        <*> optionalField' "headers" .= harLookupHeaders
        <*> optionalField' "body" .= harLookupBody

openHar :: FilePath -> Playwright Text
openHar file = do
  utilsGuid <- lookupUtilsGuid
  result <-
    sendCommand
      utilsGuid
      (MethodName "harOpen")
      (Aeson.object ["file" Aeson..= file])
  case (harOpenId result, harOpenError result) of
    (_, Just message) ->
      throwIO (harFailure message)
    (Just harId, Nothing) ->
      pure harId
    (Nothing, Nothing) ->
      throwIO (harFailure "Failed to open HAR file")

lookupUtilsGuid :: Playwright Guid
lookupUtilsGuid = do
  playwrightGuid <- lookupPlaywrightGuid
  obj <- lookupObject playwrightGuid
  initializer <- throwDecode (remoteInitializer obj)
  case playwrightUtils initializer of
    Just ref ->
      pure (channelRefGuid ref)
    Nothing ->
      throwIO (harFailure "HAR support is unavailable on this connection")

handleHarRoute :: Text -> HarRouteOptions -> Route -> Playwright ()
handleHarRoute harId opts handled = do
  request <- routeRequest handled
  url <- requestUrl request
  method <- requestMethod request
  headers <- requestAllHeaders request
  postData <- requestPostData request
  isNavigation <- requestIsNavigation request
  utilsGuid <- lookupUtilsGuid
  result <-
    sendCommand
      utilsGuid
      (MethodName "harLookup")
      ( Aeson.object
          ( [ "harId" Aeson..= harId
            , "url" Aeson..= urlToText url
            , "method" Aeson..= httpMethodToText method
            , "headers" Aeson..= fmap toNameValue headers
            , "isNavigationRequest" Aeson..= isNavigation
            ]
              -- The protocol takes postData as an optional binary, so an absent body is omitted rather than null.
              <> [ "postData" Aeson..= encodeBody body | Just body <- [postData] ]
          )
      )
  dispatchLookup result
  where
    dispatchLookup result =
      case harLookupAction result of
        "redirect"
          | Just target <- harLookupRedirectUrl result ->
              redirectRoute handled (urlFromText target)
        "fulfill" ->
          fulfillFromEntry result
        _ ->
          notFound
    notFound =
      case harRouteNotFound opts of
        HarNotFoundAbort -> abortRoute handled
        HarNotFoundFallback -> fallbackRoute handled
    fulfillFromEntry result
      | harLookupStatus result == Just (-1) =
          -- A stalled or canceled entry is left pending, as upstream does.
          pure ()
      | otherwise =
          fulfillRoute
            handled
            defaultFulfillOptions
              { fulfillStatus = Just (HttpStatus (fromMaybe 200 (harLookupStatus result)))
              , fulfillHeaders = mergeSetCookie (entryHeaders result)
              , fulfillBody = Just (FulfillBytes (decodeBody (harLookupBody result)))
              }
    entryHeaders result =
      [ HttpHeader (HeaderName (nameValueName h)) (HeaderValue (nameValueValue h))
      | h <- fromMaybe [] (harLookupHeaders result)
      ]

-- | Joins repeated set-cookie headers into one, since fulfill takes one value per name.
mergeSetCookie :: [HttpHeader] -> [HttpHeader]
mergeSetCookie = foldl step []
  where
    step acc header
      | isSetCookie header && any isSetCookie acc =
          map (\existing -> if isSetCookie existing then joinCookies existing header else existing) acc
      | otherwise =
          acc <> [header]
    isSetCookie (HttpHeader (HeaderName name) _) =
      Text.toLower name == Text.pack "set-cookie"
    joinCookies (HttpHeader name (HeaderValue first)) (HttpHeader _ (HeaderValue next)) =
      HttpHeader name (HeaderValue (first <> Text.pack "\n" <> next))

decodeBody :: Maybe Text -> ByteString
decodeBody Nothing = mempty
decodeBody (Just encoded) =
  either (const mempty) id (Base64.decode (Text.encodeUtf8 encoded))

encodeBody :: ByteString -> Text
encodeBody = Text.decodeLatin1 . Base64.encode

harFailure :: Text -> PlaywrightError
harFailure message =
  ProtocolRpc
    ProtocolFailure
      { protocolFailureName = Text.pack "Error"
      , protocolFailureMessage = message
      , protocolFailureStack = Nothing
      }

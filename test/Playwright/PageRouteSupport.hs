module Playwright.PageRouteSupport
  ( crossProcessPrefix
  , attempt
  , calledTwiceCase
  , pathOf
  , isCancelled
  , redirectChain
  , forwardChain
  , countingApp
  , subresourceRedirectApp
  , redirectChainApp
  , postRedirectApp
  , headerValue
  , refererRecordingApp
  , redirectingAssetsApp
  , assertContentTypeOfResponse
  , postCarsScript
  , optionsScript
  , optionsApp
  , recordingOkApp
  , notFoundApp
  , malformedApp
  ) where

import Control.Monad
  ( void
  )
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString
  ( ByteString
  )
import Data.Maybe
  ( isJust
  )
import Data.Text
  ( Text
  , isInfixOf
  , replace
  , takeWhileEnd
  , toLower
  )
import Network.HTTP.Types
  ( hContentType
  , hLocation
  , hReferer
  , status200
  , status302
  , status404
  )
import Network.Wai
  ( Application
  , pathInfo
  , responseLBS
  )
import Network.Wai qualified as Wai
import Network.Wai.Application.Static
  ( defaultFileServerSettings
  , staticApp
  )
import Playwright
  ( HeaderName (headerNameToText)
  , HeaderValue (headerValueToText)
  , HttpHeader (httpHeaderName, httpHeaderValue)
  , Playwright
  , Request
  , ResourceType (resourceTypeToText)
  , Route
  , UrlMatch (UrlMatchGlob)
  , defaultGotoOptions
  , requestResponse
  , responseHeaders
  , goto
  , requestRedirectedFrom
  , requestRedirectedTo
  , requestResourceType
  , requestUrl
  , route
  , urlFromText
  , urlToText
  )
import Playwright.Error
  ( PlaywrightError (RouteAlreadyHandled)
  )
import Playwright.Harness
  ( Recorded
  , recordRequest
  , assertBool
  , assertEqual
  , assertFailure
  , withAssets
  )
import Test.Tasty
  ( TestTree
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Async
  ( withAsync
  )
import UnliftIO.Exception
  ( try
  )
import UnliftIO.IORef
  ( IORef
  , readIORef
  , writeIORef
  )
import UnliftIO.MVar
  ( newEmptyMVar
  , putMVar
  , takeMVar
  )

-- | The same server reached through `localhost`, a different site from `127.0.0.1`.
crossProcessPrefix :: Text -> Text
crossProcessPrefix =
  replace "127.0.0.1" "localhost"

-- | A Playwright error from an action that is expected to fail, ignored here.
attempt :: Playwright a -> Playwright ()
attempt action = do
  outcome <- try (void action) :: Playwright (Either PlaywrightError ())
  case outcome of
    Left _ ->
      pure ()
    Right () ->
      pure ()

-- | Hands the first route to the test thread, runs the action on it, then checks
-- that a second action on the same route raises RouteAlreadyHandled.
calledTwiceCase :: String -> (Route -> Playwright ()) -> TestTree
calledTwiceCase label act =
  testCase ("route." <> label <> " should throw if called twice") $
    withAssets $ \prefix page -> do
      routes <- newEmptyMVar
      route page (UrlMatchGlob "**/*") (putMVar routes)
      withAsync (attempt (goto page (urlFromText (prefix <> "/empty.html")) defaultGotoOptions)) $ \_ -> do
        handled <- takeMVar routes
        act handled
        second <- try (act handled)
        assertEqual "second call" (Left RouteAlreadyHandled) second

-- | The last path segment of a URL, with its leading slash.
pathOf :: Text -> Text
pathOf url =
  "/" <> takeWhileEnd (/= '/') url

-- | Whether a failure reason says the request was cancelled or aborted.
isCancelled :: Text -> Bool
isCancelled reason =
  any (`isInfixOf` toLower reason) ["cancelled", "aborted"]

-- | Walks redirectedFrom back to the first request of the chain.
redirectChain :: Request -> Playwright [Request]
redirectChain request = do
  previous <- requestRedirectedFrom request
  case previous of
    Nothing ->
      pure [request]
    Just before ->
      (request :) <$> redirectChain before

-- | Follows redirectedTo from a request, collecting each resource type and path.
forwardChain :: Request -> Playwright [(Text, Text)]
forwardChain request = do
  kind <- resourceTypeToText <$> requestResourceType request
  path <- pathOf . urlToText <$> requestUrl request
  next <- requestRedirectedTo request
  rest <- maybe (pure []) forwardChain next
  pure ((kind, path) : rest)

-- | Serves the test assets, counting each /zzz request.
countingApp :: IORef Int -> Application
countingApp counter request respond =
  case pathInfo request of
    ["zzz"] -> do
      n <- readIORef counter
      writeIORef counter (n + 1)
      respond (responseLBS status200 [(hContentType, "text/plain")] (LBS8.pack (show (n * 11))))
    ["empty.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ ->
      respond (responseLBS status404 [] "")

-- | Serves a page whose stylesheet redirects three times before it is answered.
subresourceRedirectApp :: Application
subresourceRedirectApp request respond =
  case pathInfo request of
    ["one-style.html"] ->
      respond (responseLBS status200 [(hContentType, "text/html")] "<link rel=\"stylesheet\" href=\"one-style.css\"><div>hello</div>")
    ["one-style.css"] -> redirectTo "two-style.css"
    ["two-style.css"] -> redirectTo "three-style.css"
    ["three-style.css"] -> redirectTo "four-style.css"
    ["four-style.css"] ->
      respond (responseLBS status200 [(hContentType, "text/css")] "body {box-sizing: border-box; }")
    _ -> respond (responseLBS status404 [] "")
  where
    redirectTo target =
      respond (responseLBS status302 [(hLocation, target)] "")

-- | Redirects /non-existing-page.html through three more pages to /empty.html.
redirectChainApp :: Application
redirectChainApp request respond =
  case pathInfo request of
    ["non-existing-page.html"] -> redirectTo "non-existing-page-2.html"
    ["non-existing-page-2.html"] -> redirectTo "non-existing-page-3.html"
    ["non-existing-page-3.html"] -> redirectTo "non-existing-page-4.html"
    ["non-existing-page-4.html"] -> redirectTo "empty.html"
    ["empty.html"] -> respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ -> respond (responseLBS status404 [] "")
  where
    redirectTo target =
      respond (responseLBS status302 [(hLocation, target)] "")

-- | Answers POST /rredirect with a 302 to /empty.html, as the upstream test's
-- server.setRedirect does.
postRedirectApp :: Application
postRedirectApp request respond =
  case pathInfo request of
    ["rredirect"] -> respond (responseLBS status302 [(hLocation, "/empty.html")] "")
    ["empty.html"] -> respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ -> respond (responseLBS status404 [] "")

headerValue :: Text -> [HttpHeader] -> Maybe Text
headerValue name headers =
  case fmap (headerValueToText . httpHeaderValue)
    ( filter
        ( \header ->
            toLower (headerNameToText (httpHeaderName header)) == name
        )
        headers
    ) of
    [value] ->
      Just value
    _ ->
      Nothing

-- | Serves grid.html and records the Referer header the server received.
refererRecordingApp :: IORef (Maybe ByteString) -> Application
refererRecordingApp captured request respond =
  case pathInfo request of
    ["grid.html"] -> do
      writeIORef captured (lookup hReferer (Wai.requestHeaders request))
      respond (responseLBS status200 [(hContentType, "text/html")] "")
    _ ->
      respond (responseLBS status404 [] "")

-- | Redirects /logo.png to /pptr.png, and serves every other path from the assets
-- directory.
redirectingAssetsApp :: FilePath -> Application
redirectingAssetsApp assets request respond =
  case pathInfo request of
    ["logo.png"] ->
      respond (responseLBS status302 [(hLocation, "/pptr.png")] "")
    _ ->
      staticApp (defaultFileServerSettings assets) request respond

-- | Checks that the response of the recorded request has a content type header.
assertContentTypeOfResponse :: Maybe Request -> Playwright ()
assertContentTypeOfResponse recorded =
  case recorded of
    Nothing ->
      assertFailure "no request was routed"
    Just request -> do
      response <- requestResponse request
      case response of
        Nothing ->
          assertFailure "no response"
        Just value ->
          assertBool "content-type" . isJust . headerValue "content-type" =<< responseHeaders value

-- | A POST to example.com/cars with a JSON body, which resolves to the parsed
-- JSON, with extra fetch options such as credentials.
postCarsScript :: Text -> Text
postCarsScript options =
  "async () => JSON.stringify(await (await fetch('https://example.com/cars', {method: 'POST', headers: {'Content-Type': 'application/json'}, mode: 'cors', body: JSON.stringify({number: 1}), "
    <> options
    <> "})).json())"

-- | Fetches the URL with an OPTIONS and then a GET, and returns both bodies as JSON.
optionsScript :: Text -> Text
optionsScript url =
  "async () => { const url = '" <> url <> "'; const first = await (await fetch(url, {method: 'OPTIONS'})).text(); const second = await (await fetch(url, {method: 'GET'})).text(); return JSON.stringify([first, second]); }"

-- | Answers an OPTIONS with a body of Hello and everything else with World, both
-- allowing any origin. Every request is recorded.
optionsApp :: IORef [Recorded] -> Application
optionsApp requests request respond = do
  recordRequest requests request
  if Wai.requestMethod request == "OPTIONS"
    then
      respond
        ( responseLBS
            status200
            [ ("Access-Control-Allow-Origin", "*")
            , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS, DELETE")
            , ("Access-Control-Allow-Headers", "*")
            , ("Cache-Control", "no-cache")
            ]
            "Hello"
        )
    else respond (responseLBS status200 [("Access-Control-Allow-Origin", "*")] "World")

-- | Records every request, and answers with an empty page.
recordingOkApp :: IORef [Recorded] -> Application
recordingOkApp requests request respond = do
  recordRequest requests request
  respond (responseLBS status200 [(hContentType, "text/html")] "")

-- | Answers every path with a 404.
notFoundApp :: Application
notFoundApp _ respond =
  respond (responseLBS status404 [] "")

-- | Answers the raw target /malformed?rnd=%911 with an empty 200, and everything
-- else with a 404. The raw target is used because the decoded path loses the escape.
malformedApp :: Application
malformedApp request respond =
  if Wai.rawPathInfo request <> Wai.rawQueryString request == "/malformed?rnd=%911"
    then respond (responseLBS status200 [] "")
    else respond (responseLBS status404 [] "")

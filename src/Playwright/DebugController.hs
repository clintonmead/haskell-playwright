module Playwright.DebugController
  ( DebugController
  , HighlightTarget (..)
  , withDebugController
  , debugInitialize
  , debugHighlight
  , debugHideHighlight
  ) where

import Control.Concurrent.Async
  ( withAsync
  )
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( forever
  )
import Data.Aeson
  ( Value (Null, Object, String)
  , eitherDecodeStrict
  , encode
  , object
  , (.=)
  )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types
  ( parseMaybe
  , withObject
  , (.:?)
  )
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromMaybe
  )
import Data.List
  ( stripPrefix
  )
import Data.Text
  ( Text
  )
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType)
  , Socket
  , SocketType (Stream)
  , close
  , connect
  , defaultHints
  , getAddrInfo
  , socket
  )
import Network.WebSockets
  ( Connection
  , defaultConnectionOptions
  , receiveData
  , runClientWithSocket
  , sendTextData
  )
import Playwright.Driver
  ( DriverPaths
  , driverCli
  , driverNode
  )
import System.IO
  ( hGetLine
  )
import System.Process.Typed
  ( createPipe
  , getStdout
  , inherit
  , proc
  , setStderr
  , setStdout
  , startProcess
  , stopProcess
  )
import System.Timeout
  ( timeout
  )
import Text.Read
  ( readMaybe
  )

-- | A client for the driver's public debug controller channel.
--
-- Requests are sent over a websocket to @run-server --mode extension@ with the
-- @?debug-controller@ query. Responses are matched to requests by id.
data DebugController = DebugController
  { dcConnection :: Connection
  , dcNextId :: MVar Int
  , dcPending :: MVar (Map.Map Int (MVar (Either Text Value)))
  }

data HighlightTarget
  = HighlightSelector Text
  | HighlightAriaTemplate Text
  deriving stock (Eq, Show)

-- | Start a driver in extension mode, connect its debug controller channel and
-- run the action while the connection is open.
withDebugController :: DriverPaths -> (DebugController -> IO a) -> IO a
withDebugController paths action =
  bracket (startProcess config) stopProcess $ \process -> do
    line <- timeout 60000000 (hGetLine (getStdout process))
    endpoint <- case line of
      Just text | Just parsed <- parseListening text -> pure parsed
      _ -> throwIO (userError ("debug controller did not report a listen endpoint: " <> show line))
    let
      (host, port, path) = endpoint
    bracket (connectSocket host port) close $ \sock ->
      runClientWithSocket sock (hostHeader host port) (path <> "?debug-controller") defaultConnectionOptions [] $ \connection -> do
      ids <- newMVar 0
      pending <- newMVar Map.empty
      let
        dc :: DebugController
        dc =
          DebugController
            { dcConnection = connection
            , dcNextId = ids
            , dcPending = pending
            }
      withAsync (readerLoop connection pending) (\_ -> action dc)
  where
    config =
      setStderr inherit
        . setStdout createPipe
        $ proc
          (driverNode paths)
          [driverCli paths, "run-server", "--mode", "extension", "--port", "0", "--path", "/"]

-- | Initialize the debug controller, as the upstream test backend does.
debugInitialize :: DebugController -> IO (Either Text Value)
debugInitialize dc =
  request
    dc
    "initialize"
    (object ["codegenId" .= ("playwright-test" :: Text), "sdkLanguage" .= ("javascript" :: Text)])

-- | Highlight a selector or an aria template.
debugHighlight :: DebugController -> HighlightTarget -> IO (Either Text Value)
debugHighlight dc target =
  request dc "highlight" $ case target of
    HighlightSelector selector -> object ["selector" .= selector]
    HighlightAriaTemplate template -> object ["ariaTemplate" .= template]

debugHideHighlight :: DebugController -> IO (Either Text Value)
debugHideHighlight dc = request dc "hideHighlight" (object [])

request :: DebugController -> Text -> Value -> IO (Either Text Value)
request dc method params = do
  requestId <- modifyMVar (dcNextId dc) (\n -> pure (n + 1, n + 1))
  slot <- newEmptyMVar
  modifyMVar_ (dcPending dc) (pure . Map.insert requestId slot)
  sendTextData
    (dcConnection dc)
    ( encode
        ( object
            [ "id" .= requestId
            , "guid" .= ("DebugController" :: Text)
            , "method" .= method
            , "params" .= params
            , "metadata" .= object []
            ]
        )
    )
  response <- timeout 30000000 (takeMVar slot)
  modifyMVar_ (dcPending dc) (pure . Map.delete requestId)
  pure (fromMaybe (Left ("timed out waiting for DebugController " <> method)) response)

readerLoop :: Connection -> MVar (Map.Map Int (MVar (Either Text Value))) -> IO ()
readerLoop connection pending = forever $ do
  message <- receiveData connection :: IO BS.ByteString
  case eitherDecodeStrict message of
    Left _ -> pure ()
    Right value -> case parseResponse value of
      -- Events (no id) are not used by this client.
      Nothing -> pure ()
      Just (responseId, result) -> do
        table <- readMVar pending
        case Map.lookup responseId table of
          Just slot -> putMVar slot result
          Nothing -> pure ()

parseResponse :: Value -> Maybe (Int, Either Text Value)
parseResponse = parseMaybe $ withObject "response" $ \o -> do
  mId <- o .:? "id"
  case mId of
    Nothing -> fail "no id"
    Just responseId -> do
      mError <- o .:? "error"
      case mError of
        Just err -> pure (responseId, Left (errorMessage err))
        Nothing -> do
          result <- fromMaybe Null <$> o .:? "result"
          pure (responseId, Right result)

errorMessage :: Value -> Text
errorMessage err = case err of
  Object o
    | Just (Object inner) <- KeyMap.lookup "error" o
    , Just (String message) <- KeyMap.lookup "message" inner ->
        message
    | Just (String value) <- KeyMap.lookup "value" o ->
        value
  _ -> "debug controller error"

-- | Parse the @Listening on ws://host:port/path@ line printed by the driver.
parseListening :: String -> Maybe (String, Int, String)
parseListening text = do
  rest <- stripPrefix "Listening on ws://" text
  let
    (hostPort, path) = break (== '/') rest
    (hostText, portText) = breakEnd (== ':') hostPort
  port <- readMaybe portText
  let
    host = stripBrackets hostText
  pure (host, port, if null path then "/" else path)

-- | The Host header must name the server as the upstream allowlist expects, so IPv6
-- literals are bracketed (for example @[::1]:33179@).
hostHeader :: String -> Int -> String
hostHeader host port
  | ':' `elem` host = "[" <> host <> "]:" <> show port
  | otherwise = host <> ":" <> show port

connectSocket :: String -> Int -> IO Socket
connectSocket host port = do
  addresses <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just (show port))
  case addresses of
    [] -> throwIO (userError ("cannot resolve debug controller host " <> host))
    (address : _) -> do
      sock <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
      connect sock (addrAddress address)
      pure sock

-- | Split at the last colon, so IPv6 literals such as [::1]:33179 parse.
breakEnd :: (Char -> Bool) -> String -> (String, String)
breakEnd p s =
  let
    (reversedTail, reversedHead) = break p (reverse s)
  in
    (reverse (drop 1 reversedHead), reverse reversedTail)

stripBrackets :: String -> String
stripBrackets s = case s of
  '[' : inner | not (null inner), last inner == ']' -> init inner
  _ -> s

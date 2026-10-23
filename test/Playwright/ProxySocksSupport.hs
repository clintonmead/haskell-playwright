module Playwright.ProxySocksSupport
  ( socksProxyUrl
  , withSocksMockServer
  ) where

import Control.Concurrent
  ( ThreadId
  , forkIO
  , killThread
  )
import Control.Exception
  ( SomeException
  , bracket
  , finally
  , try
  )
import Control.Monad
  ( forever
  , void
  )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Text
  ( Text
  , pack
  )
import Network.Socket
  ( Family (AF_INET)
  , SockAddr (SockAddrInet)
  , Socket
  , SocketOption (ReuseAddr)
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultProtocol
  , listen
  , setSocketOption
  , socket
  , socketPort
  , tupleToHostAddress
  )
import Network.Socket.ByteString qualified as NSB

-- The proxy URL to hand to Chromium for a mock SOCKS5 server on this port.
socksProxyUrl :: Int -> Text
socksProxyUrl port =
  "socks5://127.0.0.1:" <> pack (show port)

-- A minimal SOCKS5 server, in the spirit of upstream's MockSocksServer: it
-- completes the handshake for any target, then answers the first request it
-- sees with a fixed "Served by the SOCKS proxy" page and closes the socket.
withSocksMockServer :: (Int -> IO a) -> IO a
withSocksMockServer action =
  bracket openSocksServer shutdownSocksServer $ \(port, _) -> action port

openSocksServer :: IO (Int, (Socket, ThreadId))
openSocksServer = do
  listener <- socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 16
  port <- socketPort listener
  acceptor <- forkIO (acceptLoop listener)
  pure (fromIntegral port, (listener, acceptor))

shutdownSocksServer :: (Int, (Socket, ThreadId)) -> IO ()
shutdownSocksServer (_, (listener, acceptor)) = do
  killThread acceptor
  close listener

acceptLoop :: Socket -> IO ()
acceptLoop listener = forever $ do
  (conn, _) <- accept listener
  void (forkIO (serveConnection conn `finally` close conn))

serveConnection :: Socket -> IO ()
serveConnection conn =
  void (try (handshakeAndRespond conn) :: IO (Either SomeException ()))

handshakeAndRespond :: Socket -> IO ()
handshakeAndRespond conn = do
  -- Greeting: VER, NMETHODS, METHODS. Answer "no authentication required".
  greeting <- recvExact conn 2
  void (recvExact conn (fromIntegral (BS.index greeting 1)))
  NSB.sendAll conn (BS.pack [5, 0])
  -- Request: VER, CMD, RSV, ATYP, then the address and the port.
  request <- recvExact conn 4
  addrLen <- case BS.index request 3 of
    3 -> fromIntegral . BS.head <$> recvExact conn 1
    1 -> pure 4
    4 -> pure 16
    _ -> ioError (userError "socks mock: unsupported address type")
  void (recvExact conn (addrLen + 2))
  -- Reply with success and a zeroed bound address.
  NSB.sendAll conn (BS.pack ([5, 0, 0, 1] <> replicate 6 0))
  -- The first chunk is the browser's HTTP request; answer it and close.
  void (NSB.recv conn 4096)
  NSB.sendAll conn socksResponse

socksResponse :: BS.ByteString
socksResponse =
  BS8.pack
    ( "HTTP/1.1 200 OK\r\nConnection: close\r\nContent-Type: text/html\r\nContent-Length: "
        <> show (length body)
        <> "\r\n\r\n"
        <> body
    )
  where
    body :: String
    body = "<html><title>Served by the SOCKS proxy</title></html>"

recvExact :: Socket -> Int -> IO BS.ByteString
recvExact sock =
  go []
  where
    go acc n
      | n <= 0 = pure (BS.concat (reverse acc))
      | otherwise = do
          chunk <- NSB.recv sock n
          if BS.null chunk
            then ioError (userError "socks mock: connection closed")
            else go (chunk : acc) (n - BS.length chunk)

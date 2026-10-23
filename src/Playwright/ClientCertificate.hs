module Playwright.ClientCertificate
  ( ClientCertificate (..)
  , ClientCertificateSource (..)
  , LoadedClientCertificate (..)
  , loadClientCertificates
  , toBrowserClientCertificates
  , toLaunchPersistentClientCertificates
  , toRequestClientCertificates
  ) where

import Control.Exception
  ( IOException
  , throwIO
  , try
  )
import Data.ByteString
  ( ByteString
  )
import Data.ByteString qualified as ByteString
import Data.Text
  ( Text
  , pack
  )
import Playwright.Error
  ( PlaywrightError (DecodeFailure)
  )
import Playwright.Json.ProtocolBinary
  ( ProtocolBinary
  , protocolBinaryFromByteString
  )
import Playwright.Protocol qualified as Protocol
import Playwright.Url
  ( Url
  , urlToText
  )

-- | A client certificate presented to servers matching 'clientCertificateOrigin'.
-- The files are read when the context or API request context is created.
data ClientCertificate = ClientCertificate
  { clientCertificateOrigin :: Url
  , clientCertificateSource :: ClientCertificateSource
  , clientCertificatePassphrase :: Maybe Text
  }
  deriving stock (Eq, Show)

data ClientCertificateSource
  = -- | A PEM certificate and its private key.
    ClientCertificatePem FilePath FilePath
  | -- | A PKCS#12 (PFX) bundle.
    ClientCertificatePfx FilePath
  deriving stock (Eq, Show)

-- | A client certificate with its files already read into memory.
data LoadedClientCertificate = LoadedClientCertificate
  { loadedClientCertificateOrigin :: Text
  , loadedClientCertificateCert :: Maybe ByteString
  , loadedClientCertificateKey :: Maybe ByteString
  , loadedClientCertificatePfx :: Maybe ByteString
  , loadedClientCertificatePassphrase :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Reads every certificate file. Throws 'DecodeFailure' when a file cannot
-- be read.
loadClientCertificates :: [ClientCertificate] -> IO [LoadedClientCertificate]
loadClientCertificates = mapM load

load :: ClientCertificate -> IO LoadedClientCertificate
load cert = do
  (certBytes, keyBytes, pfxBytes) <-
    case clientCertificateSource cert of
      ClientCertificatePem certPath keyPath -> do
        certFile <- readFileOrFail certPath
        keyFile <- readFileOrFail keyPath
        pure (Just certFile, Just keyFile, Nothing)
      ClientCertificatePfx pfxPath -> do
        pfxFile <- readFileOrFail pfxPath
        pure (Nothing, Nothing, Just pfxFile)
  pure
    LoadedClientCertificate
      { loadedClientCertificateOrigin = urlToText (clientCertificateOrigin cert)
      , loadedClientCertificateCert = certBytes
      , loadedClientCertificateKey = keyBytes
      , loadedClientCertificatePfx = pfxBytes
      , loadedClientCertificatePassphrase = clientCertificatePassphrase cert
      }

readFileOrFail :: FilePath -> IO ByteString
readFileOrFail path = do
  result <- try (ByteString.readFile path)
  case result of
    Right bytes ->
      pure bytes
    Left err ->
      throwIO
        ( DecodeFailure
            ("Failed to read client certificate file \"" <> pack path <> "\": " <> pack (show (err :: IOException)))
        )

-- | Protocol entries for a new browser context. 'Nothing' when there are none.
toBrowserClientCertificates :: [LoadedClientCertificate] -> Maybe [Protocol.BrowserNewContextParamsClientCertificates]
toBrowserClientCertificates = nonEmpty . map toBrowser
  where
    toBrowser loaded =
      Protocol.BrowserNewContextParamsClientCertificates
        { Protocol.browserNewContextParamsClientCertificatesCert = binary (loadedClientCertificateCert loaded)
        , Protocol.browserNewContextParamsClientCertificatesKey = binary (loadedClientCertificateKey loaded)
        , Protocol.browserNewContextParamsClientCertificatesOrigin = loadedClientCertificateOrigin loaded
        , Protocol.browserNewContextParamsClientCertificatesPassphrase = loadedClientCertificatePassphrase loaded
        , Protocol.browserNewContextParamsClientCertificatesPfx = binary (loadedClientCertificatePfx loaded)
        }

-- | Protocol entries for a new API request context. 'Nothing' when there are none.
toRequestClientCertificates :: [LoadedClientCertificate] -> Maybe [Protocol.PlaywrightNewRequestParamsClientCertificates]
toRequestClientCertificates = nonEmpty . map toRequest
  where
    toRequest loaded =
      Protocol.PlaywrightNewRequestParamsClientCertificates
        { Protocol.playwrightNewRequestParamsClientCertificatesCert = binary (loadedClientCertificateCert loaded)
        , Protocol.playwrightNewRequestParamsClientCertificatesKey = binary (loadedClientCertificateKey loaded)
        , Protocol.playwrightNewRequestParamsClientCertificatesOrigin = loadedClientCertificateOrigin loaded
        , Protocol.playwrightNewRequestParamsClientCertificatesPassphrase = loadedClientCertificatePassphrase loaded
        , Protocol.playwrightNewRequestParamsClientCertificatesPfx = binary (loadedClientCertificatePfx loaded)
        }

binary :: Maybe ByteString -> Maybe ProtocolBinary
binary = fmap protocolBinaryFromByteString

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

-- | Protocol entries for a persistent browser context launch. 'Nothing' when there are none.
toLaunchPersistentClientCertificates :: [LoadedClientCertificate] -> Maybe [Protocol.BrowserTypeLaunchPersistentContextParamsClientCertificates]
toLaunchPersistentClientCertificates = nonEmpty . map toLaunch
  where
    toLaunch loaded =
      Protocol.BrowserTypeLaunchPersistentContextParamsClientCertificates
        { Protocol.browserTypeLaunchPersistentContextParamsClientCertificatesCert = binary (loadedClientCertificateCert loaded)
        , Protocol.browserTypeLaunchPersistentContextParamsClientCertificatesKey = binary (loadedClientCertificateKey loaded)
        , Protocol.browserTypeLaunchPersistentContextParamsClientCertificatesOrigin = loadedClientCertificateOrigin loaded
        , Protocol.browserTypeLaunchPersistentContextParamsClientCertificatesPassphrase = loadedClientCertificatePassphrase loaded
        , Protocol.browserTypeLaunchPersistentContextParamsClientCertificatesPfx = binary (loadedClientCertificatePfx loaded)
        }

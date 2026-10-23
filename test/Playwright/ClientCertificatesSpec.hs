module Playwright.ClientCertificatesSpec
  ( tests
  ) where

import Data.Text
  ( Text
  , isInfixOf
  , pack
  , unpack
  )
import Playwright
  ( APIRequestContext
  , APIResponse
  , ClientCertificate (ClientCertificate)
  , ClientCertificateSource (ClientCertificatePem, ClientCertificatePfx)
  , Exactness (Inexact)
  , FulfillOptions (fulfillResponse)
  , NewContextOptions (newContextClientCertificates, newContextIgnoreHTTPSErrors)
  , NewRequestOptions (newRequestClientCertificates, newRequestIgnoreHTTPSErrors)
  , Page
  , Playwright
  , UrlMatch (UrlMatchGlob)
  , apiResponseStatus
  , apiResponseText
  , apiResponseUrl
  , defaultFulfillOptions
  , defaultGotoOptions
  , defaultNewContextOptions
  , defaultNewRequestOptions
  , disposeAPIRequestContext
  , fetchGet
  , fetchRequest
  , fulfillRoute
  , getByTestId
  , getByText
  , goto
  , httpStatusToInt
  , innerText
  , isVisible
  , newRequestContextWith
  , pageRequest
  , route
  , routeRequest
  , runPlaywright
  , urlFromText
  , urlToText
  , withPageWith
  )
import Playwright.Harness
  ( assertBool
  , assertEqual
  , assertProtocolContains
  , testAssetsDir
  , withAssetsPrefix
  , withChromiumBrowser
  , withPersistentContextWith
  )
import System.IO
  ( hGetLine
  )
import System.Process
  ( CreateProcess (std_out)
  , StdStream (CreatePipe)
  , cleanupProcess
  , createProcess
  , proc
  )
import Test.Tasty
  ( TestTree
  , testGroup
  )
import Test.Tasty.HUnit
  ( testCase
  )
import UnliftIO.Exception
  ( bracket
  )

-- Official cases that stay skipped:
--
-- * fetch "validate input", browser "validate input" and persistentContext
--   "validate input": client-side validation of conflicting certificate
--   fields cannot be expressed, because the Haskell types cannot represent
--   the invalid combinations.
-- * fetch "should send no client certificate when noCertificate is set",
--   browser "should intercept TLS and send no client certificate when
--   noCertificate is set for the visited origin", persistentContext
--   "should intercept TLS and send no client certificate when noCertificate
--   is set for the visited origin", and browser "should throw when
--   noCertificate and a real certificate are both set for the same origin":
--   the driver's certificate protocol has no noCertificate field.
-- * fetch "pass with trusted client certificates and when a http proxy is
--   used" and "... when a socks proxy is used"; browser "should pass with
--   matching certificates and when a http proxy is used", "... from env",
--   "... from config but env is there", and "... when a socks proxy is
--   used": this module has no HTTP or SOCKS proxy fixture, and the env
--   proxy variables are process-wide.
-- * browser "should pass through to non-matching origin with self-signed
--   cert": needs an HTTPS fixture that the browser trusts.
-- * browser "should not intercept TLS for origins without a client
--   certificate": Response.securityDetails is not exposed by the Haskell API.
-- * browser "should pass with matching certificates when passing as
--   content" and "... in pfx format when passing as content": the API takes
--   file paths, not bytes.
-- * browser "should not hang on tls errors during TLS 1.2 handshake" and
--   "should handle TLS renegotiation with client certificates": need a
--   TLS 1.2 fixture with version limits, SNI rejection and renegotiation.
-- * browser "should have ignoreHTTPSErrors=false by default": needs a
--   server the browser does not trust, and the error text is unverified.
-- * browser "support http2", "support http2 if the browser only supports
--   http1.1", "should return target connection errors when using http2",
--   and "should handle rejected certificate in handshake with HTTP/2": no
--   HTTP/2 fixture.
tests :: TestTree
tests =
  testGroup
    "ClientCertificates"
    [ testGroup
        "fetch"
        [ testCase "should fail with no client certificates provided" fetchNoCertificatesTest
        , testCase "should keep supporting http" fetchKeepSupportingHttpTest
        , testCase "should throw with untrusted client certs" fetchUntrustedTest
        , testCase "pass with trusted client certificates" fetchTrustedTest
        , testCase "should not leak client certificate to cross-origin redirect target" fetchRedirectTest
        , testCase "pass with trusted client certificates in pfx format" fetchPfxTest
        , testCase "should throw a http error if the pfx passphrase is incorect" fetchWrongPassphraseTest
        , testCase "should fail with matching certificates in legacy pfx format" fetchLegacyPfxTest
        , testCase "should work in the browser with request interception" fetchInterceptionTest
        ]
    , testGroup
        "browser"
        [ testCase "should keep supporting http" browserKeepSupportingHttpTest
        , testCase "should fail with no client certificates" browserNoCertificatesTest
        , testCase "should fail with self-signed client certificates" browserSelfSignedTest
        , testCase "should pass with matching certificates" browserMatchingTest
        , testCase "should pass with matching certificates in pfx format" browserPfxTest
        , testCase "should fail with matching certificates in legacy pfx format" browserLegacyPfxTest
        , testCase "should throw a http error if the pfx passphrase is incorect" browserWrongPassphraseTest
        , testCase "should pass with matching certificates on context APIRequestContext instance" browserApiRequestContextTest
        , testCase "should pass with matching certificates and trailing slash" browserTrailingSlashTest
        ]
    , testGroup
        "browser persistentContext"
        [ testCase "should pass with matching certificates" persistentMatchingTest
        ]
    ]

-- Fixture: a Node HTTPS server that requests a client certificate and
-- answers 200 for a trusted one, 403 for an untrusted one and 401 without
-- one. In "redirect" mode it also starts a plain HTTPS server that answers
-- 302 to the certificate server.

data CertServer = CertServer
  { certTarget :: Text
  , certTargetOrigin :: Text
  , certRedirectUrl :: Text
  , certRedirectOrigin :: Text
  }

nodeScript :: String
nodeScript =
  unlines
    [ "const fs = require('fs');"
    , "const https = require('https');"
    , "const [mode, dir] = process.argv.slice(1);"
    , "const read = (p) => fs.readFileSync(dir + '/' + p);"
    , "const serverCert = read('server/server_cert.pem');"
    , "const serverKey = read('server/server_key.pem');"
    , "const listen = (server) => new Promise((resolve) => server.listen(0, '127.0.0.1', () => resolve(server.address().port)));"
    , "const ccServer = https.createServer({ key: serverKey, cert: serverCert, ca: [serverCert], requestCert: true, rejectUnauthorized: false }, (req, res) => {"
    , "  const sock = req.socket;"
    , "  const cert = sock.getPeerCertificate();"
    , "  let status = 401;"
    , "  let message = 'Sorry, but you need to provide a client certificate to continue.';"
    , "  if (sock.authorized) {"
    , "    status = 200;"
    , "    message = `Hello ${cert.subject.CN}, your certificate was issued by ${cert.issuer.CN}!`;"
    , "  } else if (cert.subject) {"
    , "    status = 403;"
    , "    message = `Sorry ${cert.subject.CN}, certificates from ${cert.issuer.CN} are not welcome here.`;"
    , "  }"
    , "  res.writeHead(status, { 'Content-Type': 'text/html' });"
    , "  res.end(`<div data-testid=\"message\">${message}</div>`);"
    , "});"
    , "listen(ccServer).then((ccPort) => {"
    , "  if (mode !== 'redirect') {"
    , "    console.log(`PORT ${ccPort} 0`);"
    , "    return;"
    , "  }"
    , "  const redirectServer = https.createServer({ key: serverKey, cert: serverCert }, (req, res) => {"
    , "    res.writeHead(302, { Location: `https://127.0.0.1:${ccPort}/` });"
    , "    res.end();"
    , "  });"
    , "  listen(redirectServer).then((redirectPort) => console.log(`PORT ${ccPort} ${redirectPort}`));"
    , "});"
    ]

-- | Starts the Node fixture in the given mode and hands its URLs to the
-- action. The process is stopped when the action finishes.
withCertServer :: String -> (CertServer -> IO a) -> IO a
withCertServer mode action = do
  root <- testAssetsDir
  bracket
    (createProcess (proc "node" ["-e", nodeScript, mode, root <> "/client-certificates"]) {std_out = CreatePipe})
    cleanupProcess
    ( \(_, stdoutHandle, _, _) -> do
        line <- maybe (fail "client certificate fixture has no stdout") hGetLine stdoutHandle
        case words line of
          ["PORT", ccPort, redirectPort] ->
            action
              CertServer
                { certTarget = pack ("https://127.0.0.1:" <> ccPort <> "/")
                , certTargetOrigin = pack ("https://127.0.0.1:" <> ccPort)
                , certRedirectUrl = pack ("https://127.0.0.1:" <> redirectPort <> "/redir")
                , certRedirectOrigin = pack ("https://127.0.0.1:" <> redirectPort)
                }
          _ ->
            fail ("unexpected client certificate fixture output: " <> line)
    )

-- Certificate assets, relative to the client-certificates asset directory.

data Assets = Assets
  { trustedCertFile :: FilePath
  , trustedKeyFile :: FilePath
  , trustedPfxFile :: FilePath
  , legacyPfxFile :: FilePath
  , selfSignedCertFile :: FilePath
  , selfSignedKeyFile :: FilePath
  }

loadAssets :: IO Assets
loadAssets = do
  root <- testAssetsDir
  let
    dir = root <> "/client-certificates"
  pure
    Assets
      { trustedCertFile = dir <> "/client/trusted/cert.pem"
      , trustedKeyFile = dir <> "/client/trusted/key.pem"
      , trustedPfxFile = dir <> "/client/trusted/cert.pfx"
      , legacyPfxFile = dir <> "/client/trusted/cert-legacy.pfx"
      , selfSignedCertFile = dir <> "/client/self-signed/cert.pem"
      , selfSignedKeyFile = dir <> "/client/self-signed/key.pem"
      }

trustedPem :: Assets -> Text -> ClientCertificate
trustedPem assets origin =
  ClientCertificate
    (urlFromText origin)
    (ClientCertificatePem (trustedCertFile assets) (trustedKeyFile assets))
    Nothing

selfSignedPem :: Assets -> Text -> ClientCertificate
selfSignedPem assets origin =
  ClientCertificate
    (urlFromText origin)
    (ClientCertificatePem (selfSignedCertFile assets) (selfSignedKeyFile assets))
    Nothing

trustedPfx :: Assets -> Text -> Text -> ClientCertificate
trustedPfx assets origin passphrase =
  ClientCertificate
    (urlFromText origin)
    (ClientCertificatePfx (trustedPfxFile assets))
    (Just passphrase)

legacyPfx :: Assets -> Text -> Text -> ClientCertificate
legacyPfx assets origin passphrase =
  ClientCertificate
    (urlFromText origin)
    (ClientCertificatePfx (legacyPfxFile assets))
    (Just passphrase)

-- Request and page helpers.

withFetch :: [ClientCertificate] -> (APIRequestContext -> Playwright a) -> Playwright a
withFetch certs =
  bracket
    ( newRequestContextWith
        defaultNewRequestOptions
          { newRequestIgnoreHTTPSErrors = Just True
          , newRequestClientCertificates = Just certs
          }
    )
    disposeAPIRequestContext

browserOptions :: [ClientCertificate] -> NewContextOptions
browserOptions certs =
  defaultNewContextOptions
    { newContextIgnoreHTTPSErrors = Just True
    , newContextClientCertificates = Just certs
    }

expectResponse :: Int -> Text -> APIResponse -> Playwright ()
expectResponse expectedStatus needle response = do
  assertEqual "status" expectedStatus (httpStatusToInt (apiResponseStatus response))
  body <- apiResponseText response
  assertBool ("response body should contain " <> unpack needle) (needle `isInfixOf` body)

expectUrl :: Text -> APIResponse -> Playwright ()
expectUrl expected response =
  assertEqual "url" expected (urlToText (apiResponseUrl response))

expectMessage :: Page -> Text -> Playwright ()
expectMessage page expected = do
  actual <- innerText (getByTestId page "message")
  assertEqual "message" expected actual

-- fetch

fetchNoCertificatesTest :: IO ()
fetchNoCertificatesTest =
  withCertServer "cc" $ \fixture ->
    runPlaywright $
      withFetch [] $ \request -> do
        response <- fetchGet request (urlFromText (certTarget fixture))
        expectResponse 401 "Sorry, but you need to provide a client certificate to continue." response

fetchKeepSupportingHttpTest :: IO ()
fetchKeepSupportingHttpTest = do
  assets <- loadAssets
  withAssetsPrefix $ \prefix ->
    runPlaywright $
      withFetch [trustedPem assets prefix] $ \request -> do
        response <- fetchGet request (urlFromText (prefix <> "/one-style.html"))
        expectUrl (prefix <> "/one-style.html") response
        expectResponse 200 "<div>hello, world!</div>" response

fetchUntrustedTest :: IO ()
fetchUntrustedTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    runPlaywright $
      withFetch [selfSignedPem assets (certTargetOrigin fixture)] $ \request -> do
        response <- fetchGet request (urlFromText (certTarget fixture))
        expectUrl (certTarget fixture) response
        expectResponse 403 "Sorry Bob, certificates from Bob are not welcome here." response

fetchTrustedTest :: IO ()
fetchTrustedTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    runPlaywright $
      withFetch [trustedPem assets (certTargetOrigin fixture)] $ \request -> do
        response <- fetchGet request (urlFromText (certTarget fixture))
        expectUrl (certTarget fixture) response
        expectResponse 200 "Hello Alice, your certificate was issued by localhost!" response

fetchRedirectTest :: IO ()
fetchRedirectTest = do
  assets <- loadAssets
  withCertServer "redirect" $ \fixture ->
    runPlaywright $
      -- The certificate is scoped to the redirect origin, so it must not
      -- follow the redirect to the target.
      withFetch [trustedPem assets (certRedirectOrigin fixture)] $ \request -> do
        response <- fetchGet request (urlFromText (certRedirectUrl fixture))
        expectUrl (certTarget fixture) response
        expectResponse 401 "you need to provide a client certificate" response

fetchPfxTest :: IO ()
fetchPfxTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    runPlaywright $
      withFetch [trustedPfx assets (certTargetOrigin fixture) "secure"] $ \request -> do
        response <- fetchGet request (urlFromText (certTarget fixture))
        expectUrl (certTarget fixture) response
        expectResponse 200 "Hello Alice, your certificate was issued by localhost!" response

fetchWrongPassphraseTest :: IO ()
fetchWrongPassphraseTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    runPlaywright $
      withFetch [trustedPfx assets (certTargetOrigin fixture) "this-password-is-incorrect"] $ \request ->
        assertProtocolContains "mac verify failure" (fetchGet request (urlFromText (certTarget fixture)))

fetchLegacyPfxTest :: IO ()
fetchLegacyPfxTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    runPlaywright $
      withFetch [legacyPfx assets (certTargetOrigin fixture) "secure"] $ \request ->
        assertProtocolContains "Unsupported TLS certificate" (fetchGet request (urlFromText (certTarget fixture)))

fetchInterceptionTest :: IO ()
fetchInterceptionTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      withFetch [trustedPem assets (certTargetOrigin fixture)] $ \request ->
        withPageWith browser defaultNewContextOptions {newContextIgnoreHTTPSErrors = Just True} $ \page -> do
          route page (UrlMatchGlob "**/*") $ \handled -> do
            response <- fetchRequest request =<< routeRequest handled
            fulfillRoute
              handled
              defaultFulfillOptions
                { fulfillResponse = Just response
                }
          _ <- goto page (urlFromText (certTarget fixture)) defaultGotoOptions
          expectMessage page "Hello Alice, your certificate was issued by localhost!"

-- browser

browserKeepSupportingHttpTest :: IO ()
browserKeepSupportingHttpTest = do
  assets <- loadAssets
  withAssetsPrefix $ \prefix ->
    withChromiumBrowser $ \browser ->
      withPageWith
        browser
        defaultNewContextOptions {newContextClientCertificates = Just [trustedPem assets prefix]}
        $ \page -> do
          -- Upstream also asserts the body background CSS; that check is omitted.
          _ <- goto page (urlFromText (prefix <> "/one-style.html")) defaultGotoOptions
          visible <- isVisible (getByText page "hello, world!" Inexact)
          assertBool "hello, world! should be visible" visible

browserNoCertificatesTest :: IO ()
browserNoCertificatesTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      withPageWith browser (browserOptions [trustedPem assets "https://not-matching.com"]) $ \page -> do
        _ <- goto page (urlFromText (certTarget fixture)) defaultGotoOptions
        expectMessage page "Sorry, but you need to provide a client certificate to continue."

browserSelfSignedTest :: IO ()
browserSelfSignedTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      withPageWith browser (browserOptions [selfSignedPem assets (certTargetOrigin fixture)]) $ \page -> do
        _ <- goto page (urlFromText (certTarget fixture)) defaultGotoOptions
        expectMessage page "Sorry Bob, certificates from Bob are not welcome here."

browserMatchingTest :: IO ()
browserMatchingTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      withPageWith browser (browserOptions [trustedPem assets (certTargetOrigin fixture)]) $ \page -> do
        _ <- goto page (urlFromText (certTarget fixture)) defaultGotoOptions
        expectMessage page "Hello Alice, your certificate was issued by localhost!"

browserPfxTest :: IO ()
browserPfxTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      withPageWith browser (browserOptions [trustedPfx assets (certTargetOrigin fixture) "secure"]) $ \page -> do
        _ <- goto page (urlFromText (certTarget fixture)) defaultGotoOptions
        expectMessage page "Hello Alice, your certificate was issued by localhost!"

browserLegacyPfxTest :: IO ()
browserLegacyPfxTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      assertProtocolContains "Unsupported TLS certificate" $
        withPageWith browser (browserOptions [legacyPfx assets (certTargetOrigin fixture) "secure"]) $ \_ ->
          pure ()

browserWrongPassphraseTest :: IO ()
browserWrongPassphraseTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      assertProtocolContains "Failed to load client certificate: mac verify failure" $
        withPageWith browser (browserOptions [trustedPfx assets (certTargetOrigin fixture) "this-password-is-incorrect"]) $ \_ ->
          pure ()

browserApiRequestContextTest :: IO ()
browserApiRequestContextTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      -- Upstream lists the origin twice, once with 127.0.0.1 substituted for
      -- localhost. The fixture only uses 127.0.0.1, so one entry is enough.
      withPageWith browser (browserOptions [trustedPem assets (certTargetOrigin fixture)]) $ \page -> do
        request <- pageRequest page
        response <- fetchGet request (urlFromText (certTarget fixture))
        expectResponse 200 "Hello Alice, your certificate was issued by localhost!" response

browserTrailingSlashTest :: IO ()
browserTrailingSlashTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withChromiumBrowser $ \browser ->
      withPageWith browser (browserOptions [trustedPem assets (certTarget fixture)]) $ \page -> do
        _ <- goto page (urlFromText (certTarget fixture)) defaultGotoOptions
        visible <- isVisible (getByText page "Hello Alice, your certificate was issued by localhost!" Inexact)
        assertBool "message should be visible" visible

-- browser persistentContext

persistentMatchingTest :: IO ()
persistentMatchingTest = do
  assets <- loadAssets
  withCertServer "cc" $ \fixture ->
    withPersistentContextWith (browserOptions [trustedPem assets (certTargetOrigin fixture)]) $ \_ page -> do
      _ <- goto page (urlFromText (certTarget fixture)) defaultGotoOptions
      expectMessage page "Hello Alice, your certificate was issued by localhost!"

# Functionality todo

Finish the Playwright API and every official test that applies to a language binding (Chromium first, then Firefox/WebKit). Skip only Node-only harness internals (listener MaxListeners warnings, `__testHookAfterPointerAction`, expect/call-log collapse) until we have an equivalent.

Next work: remaining `tests/page` specs that do not need `expect` matchers, then library specs, then browsers.

## Browsers and harness

- Firefox and WebKit (suite is Chromium-only).
- HTTPS test server (`httpsServer`, ignoreHTTPSErrors, client certificates). Blocks official SSL `page.goto` cases and autowaiting “links which do not commit navigation”.
- `waitForURL` / `route` globs are not resolved against context `baseURL`.
- Regex URL matches for routes.
- IndexedDB and WebAuthn credentials on `StorageState`.
- `unrouteAll` `wait` / `ignoreErrors`.
- Tracing, HAR, video, screencast, WebSocket routing. Global `APIRequest.newContext` / `global-fetch` covers the HTTP cases (methods, dispose, userAgent, timeout, extra headers, credentials, empty body, abort, redirects, JSON bodies, maxRedirects, failOnStatusCode). `global-fetch-cookie` covers path filtering, localhost secure cookies, expiry/max-age, Set-Cookie overrides, and storageState round-trip (not IndexedDB or file path). Still open: HTTPS/`ignoreHTTPSErrors`/`securityDetails`, DNS `__testHookLookup` (custom cookie domains), brotli Range, connectOverCDP, bool/numeric params, `close` reason on fetch errors, JS `inspect`/`toJSON()` client serialization. `response.serverAddr` is wrapped but Playwright 1.61.1 omits it when Node happy-eyeballs has not set `remoteAddress` yet.

## Assertions

Official `expect-*`, `page-aria-snapshot*`, `matchers.misc`, and locator `asLocator` need Playwright `expect` (toHaveText, toHaveURL, aria snapshots, …).

## `tests/page` with no Haskell spec yet

- page-click-during-navigation (JS `__testHookAfterStable` only)
- page-evaluate-no-stall (JS `toImpl` / nonStallingRawEvaluate — not a public API)
- page-leaks (JS `toImpl` / leakedJSHandles)
- page-listeners (JS `removeAllListeners` wait/ignoreErrors)

## Ported `tests/page` with remaining cases

- **page-goto:** bad SSL (and after redirects / cross-origin); `networkidle2` invalid option (typed `LoadState` has no such constructor); Node `process` leak-listener tests; raw main-resource buffer (official Chromium fail); error message that includes HTTPS URL.
- **page-autowaiting-no-hang:** first official case needs `httpsServer`. In-flight abort uses Warp throwing `ResourceVanished` (Node `socket.destroy()`).
- **page-autowaiting-basic:** skip official internal hooks / expect call-log tests.
- **page-click:** skip Chromium-fixme fixed-position iframe, Node `__testHook*` navigation/detachment/noAutoWaiting hooks, dblclick trial, and the racy geolocation permission-popup overlay.
- **page-click-timeout-1 / 3 / 4:** skip `__testHookBeforePointerAction` / `__testHookBeforeHitTarget` cases (Node-only internals). Protocol timeout errors are `Timeout <n>ms exceeded` (the JS client wraps call-log text such as “element is not enabled”).
- **selectors-text:** skip locator regex `hasText` that needs extra escaping beyond `/pattern/flags`. String `hasText` and simple `/^…$/` filters are ported.
- **selectors-role:** skip `getByRole` regex `name` (locator `role=…[name=/…/]` is covered). Skip `description`.
- **retarget:** skip Playwright `expect` matchers (`toBeEnabled`, `toHaveValue`, `toHaveText`, `toHaveClass`, …). Core locator/handle/page checks and `waitForElementState` are ported. Official `giveItAChanceToResolve` is `rafraf(page, 5)` (double `requestAnimationFrame`, not a sleep).
- **interception:** skip Node `iso.globToRegexPattern` / `urlMatches` unit tests, unbalanced-brace glob validation (Haskell `UrlGlob` does not reject `{`), `page.route` with a `vm` RegExp, and blob-URL intercept (official WebKit-only). The `http://localhos**?*oo` case uses `localhost` like other cross-host ports.
- **page-add-locator-handler:** skip Playwright `expect` (`toBeVisible`, `toHaveScreenshot`). Protocol timeout errors now include the driver call log (so “locator handler has finished…” / intercept messages are visible). Official `window.builtins.setTimeout` is provided via `addInitScript` (test-runner-only on Node).
- **page-request-intercept:** skip Android loopback rewrite and Firefox-only multipart file upload (`fixme` on Chromium). Protocol fetch timeouts are `Timeout <n>ms exceeded` (JS prefixes `route.fetch:`). Warp static `title.html` is `text/html` (Node test server adds `charset=utf-8`).
- **locator-query:** skip regex `hasText` / `getByRole` `name` quote-escaping suites, `getByTestId(/regex/)`, same-frame constructor errors, and Node `__testHookBeforeQuery` count-during-navigation.
- **locator-misc-2:** skip locator screenshot snapshot.
- **page-event-request:** service worker / intercept-race / Chromium 204 fixme leftovers.
- **page-event-crash:** `chrome://crash` `goto` is fired in the background; awaiting it can hang the driver.
- **downloads:** createReadStream / PDF / `artifactsDir`.
- **page-pdf:** remaining `PdfOptions` (format, margin, …).

## `tests/library` with no or thin coverage

Most of `tests/library` is still open, including: browserType launch-server/connect, HAR, tracing, video, webauthn, leftover `browsercontext-fetch` (HTTPS, DNS `__testHookLookup`, brotli, connectOverCDP), headful, launcher, channels, client-certificates, and the rest of the 100 library specs. `browser` is ported (skip Node-only `__testHookBeforeSetStorageState` / `toImpl`). `browsertype-basic` is ported (skip non-Chromium CDP). `defaultbrowsercontext-1` / `defaultbrowsercontext-2` are ported (`launchPersistentContext`; skip HTTPS, Node `__testHook*` / `toImpl` defaultArgs / HAR / coverage). `proxy` / `browsercontext-proxy` are ported (skip forwarding-proxy local-network/SOCKS/HTTPS/WebSocket cases). `browsertype-launch` is ported (skip untyped `userDataDir`/`port` on launch, Node `__testHook*` / `await using`). `downloads-path` is ported (`LaunchOptions` `downloadsPath`). `oopif` is ported (skip CDP `assertOOPIFCount` / sessions, headed google.com, screenshot snapshot, connectOverCDP, official viewport fixme). `hit-target` is ported. `locator-dispatchevent-touch` is ported (`hasTouch` plus `dispatchEventWith` touch points). `unroute-behavior` is ported for default `unrouteAll` plus close / continue-after-close (skip handler-identity `unroute` and `behavior: wait | ignoreErrors`). `browsercontext-clearcookies` is ported (skip partitioned CHIPS / HTTPS). `browsercontext-basic` is ported (skip Chromium-fixme `onLine` across navigations, and `_channel.defaultUserAgentForTest`). `popup` is ported (click-to-close can still surface protocol `TargetClosedError`; the JS client often swallows that. Skip `exposeBinding` + `source.page.isClosed()` until bindings carry a page). `capabilities` is ported (skip HTTPS SharedArrayBuffer, WebSocket server cases, video/audio/`file:` range requests, `showDirectoryPicker`, and avif — `rgb.avif` is not in the 1.61.1 asset tree). `resource-timing` is ported (skip SSL). `page-event-crash` is ported. `shared-worker` and `beforeunload` are ported. `geolocation` covers isolate/context options/watchPosition/popup (skip missing-field TypeScript type errors). `page-close` covers dialog, locator-handler reason, network waiters, disconnect, mouse-during-close, request.response/allHeaders, popup navigation, exposeFunction, response.finished, and route continue while/after close (skip expect toHaveText).

## Other official trees (later)

`tests/android`, `tests/electron`, `tests/installation`, component-testing, Trace Viewer UI. Language-binding work should cover library + page (+ config as needed) before those.

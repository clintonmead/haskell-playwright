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
- Tracing, HAR, video, screencast, WebSocket routing, APIRequestContext (`context.request` / global fetch).

## Assertions

Official `expect-*`, `page-aria-snapshot*`, `matchers.misc`, and locator `asLocator` need Playwright `expect` (toHaveText, toHaveURL, aria snapshots, …).

## `tests/page` with no Haskell spec yet

- page-click-during-navigation (JS `__testHookAfterStable` only)
- page-evaluate-no-stall (JS `toImpl` / nonStallingRawEvaluate — not a public API)
- page-leaks (JS `toImpl` / leakedJSHandles)
- page-listeners (JS `removeAllListeners` wait/ignoreErrors)
- page-request-intercept

## Ported `tests/page` with remaining cases

- **page-goto:** bad SSL (and after redirects / cross-origin); `networkidle2` invalid option (typed `LoadState` has no such constructor); Node `process` leak-listener tests; raw main-resource buffer (official Chromium fail); error message that includes HTTPS URL.
- **page-autowaiting-no-hang:** first official case needs `httpsServer`. In-flight abort uses Warp throwing `ResourceVanished` (Node `socket.destroy()`).
- **page-autowaiting-basic:** skip official internal hooks / expect call-log tests.
- **page-click-scroll:** skip official Chromium fixme `display:contents` + `position` (`DOM.getBoxModel`).
- **page-click-timeout-1 / 3 / 4:** skip `__testHookBeforePointerAction` / `__testHookBeforeHitTarget` cases (Node-only internals). Protocol timeout errors are `Timeout <n>ms exceeded` (the JS client wraps call-log text such as “element is not enabled”).
- **selectors-text:** skip locator `{ hasText }` / regex hasText (no `filterHasText` yet).
- **selectors-role:** skip `getByRole` regex `name` (locator `role=…[name=/…/]` is covered). Skip `description`.
- **retarget:** skip Playwright `expect` matchers (`toBeEnabled`, `toHaveValue`, `toHaveText`, `toHaveClass`, …). Core locator/handle/page checks and `waitForElementState` are ported. Official `giveItAChanceToResolve` is `rafraf(page, 5)` (double `requestAnimationFrame`, not a sleep).
- **interception:** skip Node `iso.globToRegexPattern` / `urlMatches` unit tests, unbalanced-brace glob validation (Haskell `UrlGlob` does not reject `{`), `page.route` with a `vm` RegExp, and blob-URL intercept (official WebKit-only). The `http://localhos**?*oo` case uses `localhost` like other cross-host ports.
- **page-add-locator-handler:** skip Playwright `expect` (`toBeVisible`, `toHaveScreenshot`). Protocol timeout errors now include the driver call log (so “locator handler has finished…” / intercept messages are visible). Official `window.builtins.setTimeout` is provided via `addInitScript` (test-runner-only on Node).
- **page-history:** bfcache, reload during renderer navigation, goBack/goForward during submit, issue 20791, popup reload isolation.
- **page-event-request:** service worker / intercept-race / Chromium 204 fixme leftovers.
- **downloads:** saveAs / cancel.
- **page-pdf:** remaining `PdfOptions` (format, margin, …).

## `tests/library` with no or thin coverage

Most of `tests/library` is still open, including: browser / browserType launch-server/connect, default browser context, proxy, HAR, tracing, video, webauthn, fetch, popup, crash, headful, launcher, unroute-behavior, beforeunload, capabilities, channels, client-certificates, and the rest of the 100 library specs.

## Other official trees (later)

`tests/android`, `tests/electron`, `tests/installation`, component-testing, Trace Viewer UI. Language-binding work should cover library + page (+ config as needed) before those.

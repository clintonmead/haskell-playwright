# Functionality done

Haskell client for Playwright 1.61.1. Speaks the channel protocol to the official Node `cli.js run-driver`. Chromium only so far.

## Infrastructure

- Dual build: Stack LTS 24.58 (GHC 9.10.3) and Cabal, via Nix flake (`playwright-driver`, protocol YAML, test assets).
- Protocol types are generated at compile time (`playwright-codegen`); not vendored.
- Handshake uses `sdkLanguage: javascript` / `PW_LANG_NAME=javascript`. NixOS Chromium uses `chromiumSandbox = false`.
- Test runner: `playwright-test` (Nix shell). Tasty `-p` / `--pattern` selects tests. No extra args shards spec groups across processes (`PLAYWRIGHT_TEST_JOBS`, default `nproc/2` capped at 8). Each process is `--num-threads=1`.

## Public API (Chromium)

Core objects and the usual Page / Frame / Locator / ElementHandle / JSHandle / Request / Response / BrowserContext methods used by the ported official tests, including:

- Launch, new context/page, close, cookies, storage state (cookies + local/session storage; not IndexedDB/WebAuthn), permissions, geolocation, locale, timezone, extra headers, CSP, credentials, offline, viewport, init scripts, expose function/binding, clock.
- Navigation: `goto` (always sends `waitUntil`, default `load`), `reload`, `goBack`/`goForward`, `waitForNavigation` / `waitForURL` / `waitForLoadState` / `waitForFrameNavigated` / `waitForFrameDetached` / `waitForLoadEvent` (`armLoadEvent` does not consume buffered `loadstate`).
- Routing: page and context `route` / `fallback` / `continue` / `fulfill` / `abort` / `unrouteAll` (default behavior; wait/ignoreErrors not implemented).
- Network events: `onRequest` / `onResponse` / `onRequestFailed` / `onRequestFinished`, `waitForRequest` / `waitForResponse`, `page.requests`, sizes, postData JSON, redirects.
- Query: `querySelector` / `querySelectorAll` (`page.$` / `page.$$`), `evalOnSelectorBool`.
- Downloads (`waitForDownload` / `downloadPath`), file chooser + `setInputFiles` payloads, PDF (`tagged`/`outline`), screenshot, workers, dialogs, console, pageerror, popups.
- Locator click/fill/check/hover/select, `clickWith` (`noWaitAfter`, `force`, `position`, `timeout`), `clickHandleWith`, and `dropOnto` (in-memory file payloads and clipboard `data`; not host `localPaths`).

## Official tests with a Haskell port

Ports live under `test/Playwright/` and are registered in `test/Main.hs`. Names match official groups (`page-goto`, `page-autowaiting-basic`, …). Coverage inside a spec is often a subset of the TypeScript file.

### `tests/page`

elementhandle-bounding-box, elementhandle-click, elementhandle-content-frame, elementhandle-convenience, elementhandle-eval-on-selector, elementhandle-misc, elementhandle-owner-frame, elementhandle-press, elementhandle-query-selector, elementhandle-screenshot, elementhandle-scroll-into-view, elementhandle-select-text, elementhandle-type, elementhandle-wait-for-element-state, eval-on-selector, eval-on-selector-all, frame-evaluate, frame-frame-element, frame-goto, frame-hierarchy, jshandle-as-element, jshandle-evaluate, jshandle-json-value, jshandle-properties, jshandle-to-string, locator-click, locator-convenience, locator-element-handle, locator-evaluate, locator-frame, locator-is-visible, locator-list, locator-misc-1, locator-misc-2, locator-query, network-post-data, page-add-init-script, page-add-script-tag, page-add-style-tag, page-autowaiting-basic, page-autowaiting-no-hang, page-basic, page-check, page-click, page-click-react, page-click-scroll, page-click-timeout-1, page-click-timeout-2, page-click-timeout-3, page-click-timeout-4, page-dialog, page-dispatchevent, page-drag, page-drop, page-emulate-media, page-evaluate, page-evaluate-handle, page-event-console, page-event-load, page-event-network, page-event-pageerror, page-event-popup, page-event-request, page-expose-function, page-filechooser, page-fill, page-focus, page-goto, page-history, page-keyboard, page-localstorage, page-mouse, page-navigation, page-network-idle, page-network-request, page-network-response, page-network-sizes, page-request-continue, page-request-fallback, page-request-fulfill, page-request-gc, page-route, page-screenshot, page-select-option, page-set-content, page-set-extra-http-headers, page-set-input-files, page-strict, page-wait-for-function, page-wait-for-load-state, page-wait-for-navigation, page-wait-for-request, page-wait-for-response, page-wait-for-selector-1, page-wait-for-selector-2, page-wait-for-url, queryselector, selectors-css, selectors-get-by, selectors-misc, selectors-role, selectors-text.

### `tests/library` (partial ports)

browsercontext-add-cookies, browsercontext-add-init-script, browsercontext-base-url, browsercontext-cookies, browsercontext-credentials, browsercontext-csp, browsercontext-expose-function, browsercontext-locale, browsercontext-page-event, browsercontext-pages, browsercontext-route, browsercontext-set-extra-http-headers, browsercontext-storage-state, browsercontext-timezone-id, browsercontext-viewport, download, geolocation, page-clock, page-close, pdf, permissions, tap (hasTouch).

Plus connection/transport unit tests and a small integration spec.

Known holes inside otherwise-ported specs are listed in `FUNCTIONALITY_TODO.md`.

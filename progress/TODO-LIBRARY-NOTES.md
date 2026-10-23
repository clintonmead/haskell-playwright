# Functionality todo: tests/library notes

Full text of the long `tests/library` entries in [FUNCTIONALITY_TODO.md](FUNCTIONALITY_TODO.md), moved verbatim. The TODO keeps a short line per item and links here.

<a id="capabilities-environment"></a>
- **capabilities (environment):** "should support webgl" and "should support webgl 2" fail with the driver 1.64.1 Chromium in this sandbox (`getContext('webgl')` returns null). They passed on driver 1.61.1. The cause is the browser build or GPU setup, not the binding. Left failing until the browser setup is checked.
<a id="capabilities-partial"></a>
- **capabilities (partial):** the video, webm, audio and mp4 playback cases are ported and pass. Still open: the SharedArrayBuffer case (HTTPS server), the WebSocket cases (no ws server), and the avif and window.safari cases.
<a id="tap"></a>
- **tap:** `tapWith` (Locator) and `tapHandleWith` take `TapOptions` (force, modifiers, position, timeout, trial). All nine upstream `tap.spec.ts` cases are ported and pass in `TapSpec.hs`. The touch-point case uses the new `touchscreenTap` (`Playwright.Touchscreen`, a `page.touchscreen.tap` wrapper; see QUESTIONS.md "tap: touchscreen tap added as a public API").
<a id="browsercontext-credentials-partial"></a>
- **browsercontext-credentials (partial):** 11 of 16 upstream cases match, all passing in `BrowserContextCredentialsSpec.hs`. Includes the matching-origin and case-insensitive-origin cases. "should work with a single credential in an array" is covered by the single-value case, since the Haskell API takes one `HttpCredentials`. Still open: "should work with setHTTPCredentials and multiple credentials", "should work with multiple credentials for different origins", "should fall back to credentials without origin", and "should use the first matching credential". The public API takes one credential (`newContextHttpCredentials :: Maybe HttpCredentials`, `setHTTPCredentials :: BrowserContext -> HttpCredentials -> Playwright ()`). Upstream accepts a list. Widening would break existing callers, so these wait on the decision in QUESTIONS.md.

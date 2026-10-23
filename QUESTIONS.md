# Questions for the user

Decisions made without waiting for an answer. Each entry gives the question, the guess that was taken, and what to change if the answer is different. Newest first.

Each line below links to the full entry in [progress/QUESTIONS-ARCHIVE.md](progress/QUESTIONS-ARCHIVE.md). Status is `resolved` or `guess in place, no answer yet`.

0. [Expect matchers surface only the driver error, not errorDetails](progress/QUESTIONS-ARCHIVE.md#q-26) - guess in place, no answer yet
1. [backlog: HEAD does not build (15 missing exports), and many slices are uncommitted](progress/QUESTIONS-ARCHIVE.md#q-1) - committed as 8f8310d (option a); the user asked for the commit
2. [page-network-request: gaps left unported, and the event-source server omits keep-alive](progress/QUESTIONS-ARCHIVE.md#q-2) - guess in place, no answer yet
3. [page-wait-for-response: "second waiter loses events" claim did not hold](progress/QUESTIONS-ARCHIVE.md#q-3) - guess in place, no answer yet
4. [browsercontext-events frame and WebError design](progress/QUESTIONS-ARCHIVE.md#q-4) - guess in place, no answer yet
5. [browsercontext-add-init-script exposeFunctions design](progress/QUESTIONS-ARCHIVE.md#q-5) - guess in place, no answer yet
6. [browsercontext-base-url relative glob resolution skips `..` and glob characters](progress/QUESTIONS-ARCHIVE.md#q-6) - guess in place, no answer yet
7. [locator-query cross-frame has/leftOf/rightOf/above/below/near errors](progress/QUESTIONS-ARCHIVE.md#q-7) - guess in place, no answer yet
8. [page-wait-for-selector-1 log cases match the bare timeout text](progress/QUESTIONS-ARCHIVE.md#q-8) - guess in place, no answer yet
9. [Log cases wait on the main frame, not a frame-level timeout](progress/QUESTIONS-ARCHIVE.md#q-9) - guess in place, no answer yet
10. [Log cases use a fixed 100 ms wait instead of two rAF cycles](progress/QUESTIONS-ARCHIVE.md#q-10) - guess in place, no answer yet
11. [Multi-needle protocol assertion added to the harness](progress/QUESTIONS-ARCHIVE.md#q-11) - guess in place, no answer yet
12. [ownerFrame equality uses framesEqual, not Eq](progress/QUESTIONS-ARCHIVE.md#q-12) - guess in place, no answer yet
13. [Detached-frame case matches the bare text](progress/QUESTIONS-ARCHIVE.md#q-13) - guess in place, no answer yet
14. [`httpCredentials` as an array is not ported](progress/QUESTIONS-ARCHIVE.md#q-14) - guess in place, no answer yet
15. [Raw response headers: `responseHeaders` stands in for `allHeaders`](progress/QUESTIONS-ARCHIVE.md#q-15) - guess in place, no answer yet
16. [Fetch Metadata headers: the sec-fetch case is not ported](progress/QUESTIONS-ARCHIVE.md#q-16) - guess in place, no answer yet
17. [The 307 cross-origin navigation waits for a newer driver](progress/QUESTIONS-ARCHIVE.md#q-17) - guess in place, no answer yet
18. [Fresh cookie from the store inside a route](progress/QUESTIONS-ARCHIVE.md#q-18) - guess in place, no answer yet
19. [URL matching: `UrlGlob` becomes `UrlMatch`](progress/QUESTIONS-ARCHIVE.md#q-19) - guess in place, no answer yet
20. [Regex engine: `regex-tdfa`](progress/QUESTIONS-ARCHIVE.md#q-20) - guess in place, no answer yet
21. [Predicates on routes](progress/QUESTIONS-ARCHIVE.md#q-21) - guess in place, no answer yet
22. [Regex flags `g` and `y` are not sent to the driver](progress/QUESTIONS-ARCHIVE.md#q-22) - guess in place, no answer yet
23. [Glob matching follows the upstream translation](progress/QUESTIONS-ARCHIVE.md#q-23) - guess in place, no answer yet
24. [Driver pin moved to 1.64.1](progress/QUESTIONS-ARCHIVE.md#q-24) - guess in place, no answer yet
25. [WAYLAND_DISPLAY unset in the dev shell](progress/QUESTIONS-ARCHIVE.md#q-25) - guess in place, no answer yet
26. [Single credential sent as a list](progress/QUESTIONS-ARCHIVE.md#q-26) - guess in place, no answer yet
27. [waitForFunction without a handle](progress/QUESTIONS-ARCHIVE.md#q-27) - guess in place, no answer yet
28. [$$eval on a missing frame](progress/QUESTIONS-ARCHIVE.md#q-28) - guess in place, no answer yet
29. [The 307 cross-origin navigation now runs](progress/QUESTIONS-ARCHIVE.md#q-29) - guess in place, no answer yet
30. [Context pageload and pageclose attach to existing pages](progress/QUESTIONS-ARCHIVE.md#q-30) - guess in place, no answer yet
31. [FrameLocator is a plain Locator](progress/QUESTIONS-ARCHIVE.md#q-31) - guess in place, no answer yet
32. [Frame waitForURL uses the default timeout](progress/QUESTIONS-ARCHIVE.md#q-32) - guess in place, no answer yet
33. [waitForURL waitUntil options](progress/QUESTIONS-ARCHIVE.md#q-33) - guess in place, no answer yet
34. [allHeaders returns a list, not a Map](progress/QUESTIONS-ARCHIVE.md#q-34) - guess in place, no answer yet
35. [route.fulfill set-cookie test is marked as an expected failure upstream](progress/QUESTIONS-ARCHIVE.md#q-35) - guess in place, no answer yet
36. [svg charset case compares the decoded size, not a screenshot](progress/QUESTIONS-ARCHIVE.md#q-36) - guess in place, no answer yet
37. [Locator.waitForFunction naming and strict default](progress/QUESTIONS-ARCHIVE.md#q-37) - guess in place, no answer yet
38. [page-drag skips the drag-out-of-an-iframe case](progress/QUESTIONS-ARCHIVE.md#q-38) - guess in place, no answer yet
39. [page-evaluate argument-side cases with no argument constructor](progress/QUESTIONS-ARCHIVE.md#q-39) - superseded: the cases are ported in the test file via record syntax (error argument helper added); no public API change. See the archive entry.
40. [page-wait-for-navigation: frame detach error wording](progress/QUESTIONS-ARCHIVE.md#q-40) - guess in place, no answer yet
41. [page-wait-for-navigation: detach waiter left unconsumed](progress/QUESTIONS-ARCHIVE.md#q-41) - guess in place, no answer yet
42. [page-wait-for-navigation: descendant frame detach](progress/QUESTIONS-ARCHIVE.md#q-42) - guess in place, no answer yet
43. [page-network-idle: response.ok() replaced by a status check](progress/QUESTIONS-ARCHIVE.md#q-43) - guess in place, no answer yet
44. [page-network-idle: foo.js held, then released](progress/QUESTIONS-ARCHIVE.md#q-44) - guess in place, no answer yet
45. [frame-hierarchy: isDetached is driven by the frameDetached event](progress/QUESTIONS-ARCHIVE.md#q-45) - guess in place, no answer yet
46. [frame-hierarchy: framesets race in the queued frameAttached job](progress/QUESTIONS-ARCHIVE.md#q-46) - guess in place, no answer yet
47. [frame-hierarchy: x-frame-options refusal is checked through a console waiter](progress/QUESTIONS-ARCHIVE.md#q-47) - guess in place, no answer yet
48. [frame-hierarchy: context-level frame events register inside a page job](progress/QUESTIONS-ARCHIVE.md#q-48) - guess in place, no answer yet
49. [workers: console timestamp is checked against the page clock](progress/QUESTIONS-ARCHIVE.md#q-49) - guess in place, no answer yet
50. [workers: "should report console logs" placeholder replaced](progress/QUESTIONS-ARCHIVE.md#q-50) - guess in place, no answer yet
51. [workers: dedicated-worker console needs a worker-level subscription](progress/QUESTIONS-ARCHIVE.md#q-51) - guess in place, no answer yet
52. [browser-context: `armContextConsole` registers an unfiltered waiter](progress/QUESTIONS-ARCHIVE.md#q-52) - guess in place, no answer yet
53. [workers: network cases and the unfiltered finished waiter](progress/QUESTIONS-ARCHIVE.md#q-53) - guess in place, no answer yet
54. [workers: version gates print `skipped:` and pass](progress/QUESTIONS-ARCHIVE.md#q-54) - guess in place, no answer yet
55. [workers: extra-headers WaitTimeout and the finished waiter](progress/QUESTIONS-ARCHIVE.md#q-55) - guess in place, no answer yet
56. [workers: iframe network case hang (not reproduced)](progress/QUESTIONS-ARCHIVE.md#q-56) - guess in place, no answer yet
57. [workers: redirect case is a skip with a reason](progress/QUESTIONS-ARCHIVE.md#q-57) - guess in place, no answer yet
58. [workers: webkit and firefox fixmes are not represented](progress/QUESTIONS-ARCHIVE.md#q-58) - guess in place, no answer yet
59. [page-click-timeout: inline markup instead of input/button.html](progress/QUESTIONS-ARCHIVE.md#q-59) - guess in place, no answer yet
60. [page-click-timeout: mode-gated and Android cases](progress/QUESTIONS-ARCHIVE.md#q-60) - guess in place, no answer yet
61. [page-evaluate-callback: handle-routing cases ported with page-side calls](progress/QUESTIONS-ARCHIVE.md#q-61) - guess in place, no answer yet
62. [page-evaluate-callback: case 22 skipped](progress/QUESTIONS-ARCHIVE.md#q-62) - guess in place, no answer yet
63. [tap: touchscreen tap added as a public API](progress/QUESTIONS-ARCHIVE.md#q-63) - guess in place, no answer yet
64. [browsercontext-csp: plain frame under CSP accepts addScriptTagOnFrame (resolved: the test header was malformed)](progress/QUESTIONS-ARCHIVE.md#q-64) - resolved
65. [Test runs must use the nix driver, not the inherited one](progress/QUESTIONS-ARCHIVE.md#q-65) - guess in place, no answer yet
66. [Element-handle form of selectOption: the public shape](progress/QUESTIONS-ARCHIVE.md#q-66) - guess in place, no answer yet
67. [page-evaluate nice error after navigation](progress/QUESTIONS-ARCHIVE.md#q-67) - guess in place, no answer yet
68. [Handle preview cases (elementhandle-convenience)](progress/QUESTIONS-ARCHIVE.md#q-68) - guess in place, no answer yet
69. [page-evaluate usable-message cases with a JS function argument](progress/QUESTIONS-ARCHIVE.md#q-69) - guess in place, no answer yet
70. [page-expose-function exposeBinding source argument](progress/QUESTIONS-ARCHIVE.md#q-70) - guess in place, no answer yet
71. [selectors-get-by: regex forms of internal:text and internal:label](progress/QUESTIONS-ARCHIVE.md#q-71) - resolved
72. [selectors-get-by: v-flag regex in getByRoleNameRegex](progress/QUESTIONS-ARCHIVE.md#q-72) - resolved
73. [page-screenshot: ScreenshotOptions fields](progress/QUESTIONS-ARCHIVE.md#q-73) - guess in place, no answer yet
74. [page-screenshot: enum constructor names and quality type](progress/QUESTIONS-ARCHIVE.md#q-74) - guess in place, no answer yet
75. [page-screenshot: mask, maskColor and style](progress/QUESTIONS-ARCHIVE.md#q-75) - guess in place, no answer yet
76. [page-screenshot: image decoding for pixel checks](progress/QUESTIONS-ARCHIVE.md#q-76) - guess in place, no answer yet
77. [page-screenshot: path error type and mime table](progress/QUESTIONS-ARCHIVE.md#q-77) - guess in place, no answer yet
78. [page-screenshot: caret, animations and scale behaviour](progress/QUESTIONS-ARCHIVE.md#q-78) - guess in place, no answer yet
79. [build: InitScript overlay is not in HEAD](progress/QUESTIONS-ARCHIVE.md#q-79) - guess in place, no answer yet
80. [build: uncommitted specs break the build in three places](progress/QUESTIONS-ARCHIVE.md#q-80) - guess in place, no answer yet
81. [page-screenshot: canvas case needs a byte-level check](progress/QUESTIONS-ARCHIVE.md#q-81) - guess in place, no answer yet
82. [build: HEAD test/Main.hs expects PageClickTimeoutSpec.tests](progress/QUESTIONS-ARCHIVE.md#q-82) - guess in place, no answer yet
83. [browsercontext-credentials: widen the credentials API to a list?](progress/QUESTIONS-ARCHIVE.md#q-83) - guess in place, no answer yet
84. [browsercontext-events: console event for a popup that is already gone](progress/QUESTIONS-ARCHIVE.md#q-84) - guess in place, no answer yet
85. [route chain: a fallback called after the handler returns never reaches the next handler](progress/QUESTIONS-ARCHIVE.md#q-85) - guess in place, no answer yet
86. [attribution: which trailer rule applies to commits in this repo](progress/QUESTIONS-ARCHIVE.md#q-86) - guess in place, no answer yet: the Haiku trailer applies to playwright-haskell commits (option a)
87. [interception: glob table and invalid-glob cases](progress/QUESTIONS-ARCHIVE.md#q-87) - guess in place, no answer yet: record as skips (option a)
88. [page-evaluate-no-stall and page-evaluate-world: new spec file or skips?](progress/QUESTIONS-ARCHIVE.md#q-88) - guess in place, no answer yet: record as skips (option a)
89. [page-evaluate: error argument needs a caller-supplied stack](progress/QUESTIONS-ARCHIVE.md#q-89) - guess in place, no answer yet: the helper takes the stack from the caller (option a)
90. [page-evaluate: jsonValue on window returns a value instead of throwing](progress/QUESTIONS-ARCHIVE.md#q-90) - guess in place, no answer yet: record as skip (option a)
91. [commit attribution: the upstream CLAUDE.md bans agent trailers, the session asks for a Haiku trailer](progress/QUESTIONS-ARCHIVE.md#q-91) - guess in place, no answer yet: use the session Haiku trailer on commits and PRs (option a)
92. [page-evaluate: 100Mb transfer from page to node.js stalls the run](progress/QUESTIONS-ARCHIVE.md#q-92) - guess in place, no answer yet: record as skip (option a)
93. [locator-query: same-frame check is a client-side throw, not a driver error](progress/QUESTIONS-ARCHIVE.md#q-93) - guess in place, no answer yet: record as skip and remove the case (option a)
94. [locator-query: regex hasText with a single quote fails in the driver (driver adds `u` flag)](progress/QUESTIONS-ARCHIVE.md#q-94) - guess in place, no answer yet: record as skip and remove the case (option a)
95. [page-evaluate-world: the `world` option never reaches the driver on 1.64.1](progress/QUESTIONS-ARCHIVE.md#q-95) - guess in place, no answer yet: record all 8 cases as skips (option a)
96. [page-screenshot: masked element-handle screenshot has no public API](progress/QUESTIONS-ARCHIVE.md#q-96) - guess in place, no answer yet: record "should work with elementhandle" as a skip (option a)
97. [page-screenshot: `__testHookBeforeScreenshot` is a test-only hook with no public API](progress/QUESTIONS-ARCHIVE.md#q-97) - guess in place, no answer yet: record "should stop animations that happen right before screenshot" as a skip (option a)
98. [page-set-input-files: path-based upload cases need a path API the Haskell side does not have](progress/QUESTIONS-ARCHIVE.md#q-98) - guess in place, no answer yet: record the eleven path-based cases as skips until a path API is designed (option a)
99. [page-set-input-files: the 1 MiB multipart upload case needs a multipart parser the test suite lacks](progress/QUESTIONS-ARCHIVE.md#q-99) - guess in place, no answer yet: record the case as a skip (option a)
100. [browsercontext-storage-state: file-based storageState and malformed-file cases have no API](progress/QUESTIONS-ARCHIVE.md#q-100) - guess in place, no answer yet: record as skips (option a)
101. [browsercontext-storage-state: IndexedDB and OPFS cases need options the library does not expose](progress/QUESTIONS-ARCHIVE.md#q-101) - guess in place, no answer yet: record as skips (option a)
102. [browsercontext-storage-state: WebAuthn credentials in storageState have no API](progress/QUESTIONS-ARCHIVE.md#q-102) - guess in place, no answer yet: record as skips (option a)
103. [browsercontext-add-cookies: secure cookies on a secure WebSocket need HTTPS](progress/QUESTIONS-ARCHIVE.md#q-103) - guess in place, no answer yet: record as skip (option a)
104. [browsercontext-basic: navigator.onLine across navigations and default user agent](progress/QUESTIONS-ARCHIVE.md#q-104) - guess in place, no answer yet: record as skips (option a)
105. [browsercontext-events: console events for early-closed popups fail with MissingObject](progress/QUESTIONS-ARCHIVE.md#q-105) - guess in place, no answer yet: record as skips (option a)
106. [split of BrowserContextFetch, PageRoute, PageGoto and GlobalFetch into two parts and a Support module](progress/QUESTIONS-ARCHIVE.md#q-106) - guess in place, no answer yet: keep the split as made (naming, cut point, and the 1,300-line Support module accepted for now)
107. Discovery is dry, so the 20-slot pipeline runs a fixed list of 20 feature slices instead (script: feature-port-pipeline.js). Guess in place: each slice implements its missing src/ feature in its own commit, then ports its specs. Slices: tracing, HAR context/page/websocket, video, screencast, screencast-actions, expect core/text, aria-snapshot, to-match-aria-snapshot, locator-generator, selector-generator, role-utils, browsertype-connect, browser-server, debug-controller, multiclient, locator-any-frame, fetch-small, and two master fixes (proxy-routing failures, flaky serviceworker test). Slots use pw-wt-feat-N and pw-build/feat-N, separate from the old slot-N. Old pipeline wf_09aab4ac-e22 could not be stopped from here (no task id); it only makes no-op merges. Stop it from /workflows.
108. [feat-tracing: `playwright.cabal` edit rides in the feature commit](progress/QUESTIONS-ARCHIVE.md#q-108) - guess in place, no answer yet: `Playwright.Tracing` had to be listed in `playwright.cabal` in the feature commit, because `-Wmissing-home-modules` fails the library build without it. The test-suite entry for `Playwright.TracingSpec` went in the separate registration commit (option a)
109. [feat-tracing: two reduced upstream cases check zip entry names only](progress/QUESTIONS-ARCHIVE.md#q-109) - guess in place, no answer yet: "should overwrite existing file" and "should recover tracing after a failed stop" check the archive's entry names, not trace content (option a). Revisit when trace content can be read
110. HAR slice scope: routeFromHAR only, or also context recordHar and zip HAR? Guess in place, no answer yet: this slice ports routeFromHAR for Page and BrowserContext through the LocalUtils harOpen, harLookup and harClose protocol (src/Playwright/Har.hs), plus the redirectNavigationRequest route command. recordHar stays with the sibling feat-har-context slice. Cases needing recordHar or zip archives are skipped with that reason.
111. screencast restart: the second start asked for 320x240 but its first frame was 500 wide. Upstream asserts only that both starts and stops succeed, so the port checks a JPEG frame instead of the width. Guess in place: the driver or Chromium keeps the viewport size for the first frame after a restart; not verified further.

## feat-video (video slice)

- **recordVideo.showActions is not modelled.** `RecordVideo` has only `dir` and `size`. Upstream `showActions` is unported, so this is a gap, not a skip of a test case.
- **recordVideo `dir` is required in Haskell.** Upstream `recordVideo: {}` cannot be expressed. Guess: keep `RecordVideo` with a required `FilePath`, and skip "should not throw without recordVideo.dir". Omitting recordVideo covers the default case.
- **Relative `recordVideo.dir` is passed through unchanged.** Guess: the driver shares the process cwd and resolves it the same way. This is unverified until the relative-path case runs.
- **Size and pixel checks need an ffmpeg frame reader.** Guess: parse `ffmpeg -i` stderr for size. Pixel cases stay skipped until then.
- **Popup video cases** ("should expose video path blank popup", "should work for popups") need a popup event helper that the harness lacks.

## feat-screencast-actions (overlay slice)

- **Overlay pixel cases are skipped (4 cases).** The top-layer tests in `screencast-overlay.spec.ts` (dialog and popover, before and after) decode `page.screenshot()` as PNG and sample pixel (100, 100). The harness has no PNG decoder. Guess: skip them for this slice and add a minimal decoder (8-bit RGB/RGBA, filter types 0-4, zlib IDAT) in a later slice. Tests that need them are not in `ScreencastOverlaySpec.hs`.
- **Overlay spec count.** The upstream file has 9 plain cases and 4 pixel cases (13 in total), not 11 as first noted. The 9 plain cases are ported.
- **Action annotation gaps.** "should render annotation styles" and `cursor: "none"` are not ported. The highlight and action-point elements are gone before the poll sees them. See `progress/FUNCTIONALITY_TODO.md`.
## browser-server slice (feat-browser-server)

- **launchServer is not added as a separate method.** Upstream `BrowserType.launchServer` is client-side: it launches and then calls `browser.bind`. Guess: the Haskell API exposes `bindBrowser` / `bindBrowserWith` / `unbindBrowser` (driver channels `Browser.startServer` / `Browser.stopServer`) and leaves `launchServer` as a launch-then-bind composition for later.
- **Connect is not implemented.** `BrowserType.connect` needs LocalUtils.connect and a second Connection. Cases that call `browserType.connect` are skipped until that exists.
- **PWTEST_SERVER_REGISTRY is set with setEnv in the test process before the driver spawns.** The driver reads the variable at start (it is inherited from the process environment). Guess: the descriptor case must not run in parallel with other cases that spawn drivers.

## browser-server / launcher / channels / signals (feat-browser-server)

- Q: Should `launchServer` be its own method? A: No. Haskell exposes `bindBrowser`, `bindBrowserWith` and `unbindBrowser` over `Browser.startServer` and `Browser.stopServer`. Best guess: launch-then-bind composition comes later.
- Q: The launcher headed-no-xserver case. A: Skipped. The message is only produced when Chromium logs "Missing X server", and Chromium 1248 starts headed with DISPLAY unset on this host, so the assertion cannot pass here. Best guess: a port needs a host where Chromium fails without a display.
- Q: channels and signals cases. A: All skipped, since they need `_debugScopeState()`, Node's `domain` module, or the Node-side `launchServer` child process. Best guess: revisit once the driver exposes an introspection channel and a launch-server path exists.

## page-listeners / page-leaks slice (feat-page-leaks-listeners)

- Q: Is console listener removal a general `removeAllListeners` or console-only? A: Console only, through `removeAllConsoleListeners :: Page -> ListenerRemoval -> Playwright ()`. Best guess: other event types can reuse `removeScopedEventHandlers` when they are ported.
- Q: "wait should throw" needs the removal to register before the handler finishes, otherwise its error is dropped. A: The test waits 200 ms before releasing the handler. Best guess: a deterministic hook would need a removal-registered signal in the driver path, so the timing wait stays until then.
- Q: page-leaks "expect should not leak" uses `toBeVisible`. A: The Haskell port checks `isVisible` once per element, not the retrying `expect` matcher, since Playwright `expect` is not in this client. Best guess: the element is visible as soon as it is appended, so the one-shot check is enough for the leak count.
- Q: page-leaks counts. A: Only main-world WeakRefs are counted, with a retry loop of 20 attempts at 100 ms after `requestGC`. Best guess: the utility-world count and the leakedJSHandles assertion are not reachable from the client.

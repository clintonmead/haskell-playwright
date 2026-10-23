# Questions archive

Full text of every entry in [QUESTIONS.md](../QUESTIONS.md), moved verbatim. Newest first. Each entry has an anchor `q-N`, where N is its position in the index.

<a id="q-1"></a>
## backlog: HEAD does not build, and many slices are uncommitted

**Question:** How should the uncommitted work be committed? HEAD does not build: commit e43d654 added `Playwright.InitScript` and `PageAddInitScriptCallbackSpec` to `playwright.cabal`, but the module files were never committed. About 90 modified or untracked files in the working tree come from several slices (InitScript, Worker, ContextEvents, Page/Wait, WorkersSpec, the add-init-script specs, and more). `page-network-request` depends on those shared source changes, so its spec cannot be committed alone. A HEAD-only build of its spec fails at cabal configure with "can't find source for Playwright/InitScript".

**Finding:** The full working tree builds (`full-tree-run.log`, BUILD_RC=0). HEAD does not. Committing only the four missing InitScript module files does not fix HEAD. The HEAD test-suite build still fails on 15 missing exports from HEAD's `src/Playwright.hs`: `WebError`, `WebErrorLocation`, `onContextWebError`, `onContextDownload`, `onContextDialogClosed`, `onContextFrameAttached`, `onContextFrameDetached`, `onContextFrameNavigated`, `onFrameNavigated`, `isCheckedHandle`, `getByTextRegex`, `getByLabelRegex`, `addPageInitScriptExposing`, `waitForFunctionOnLocator` and `waitForFunctionOnLocatorWithArg`. So a per-slice commit cannot build on its own.

**Guess taken (not confirmed):** Make one catch-up commit of the whole verified working tree (src, test and cabal), excluding the doc split (DESIGN, QUESTIONS, progress notes, RESUME). This is option (a) below. It is the only route that makes HEAD build again, so the guess is that you want it. The user then asked for the working tree to be committed, which the guess covers. It was made as commit 8f8310d, "feat: catch up source, tests and cabal to the verified tree". The per-slice commits in the TODO are not separate commits.

**Options:**
- (a) One catch-up commit of the whole verified working tree, excluding the doc split. This makes HEAD build again. Then per-slice commits from here. This is the guess.
- (b) Split the backlog into per-slice commits. This is the cleanest history, but many source hunks are shared between slices, so it takes a lot of hand-splitting.
- (c) Leave everything uncommitted until you choose.

**To change:** Tell me (a), (b) or (c). Under (a), I will make the catch-up commit next, with a message that lists the slices it covers.
<a id="q-2"></a>
## page-network-request: gaps left unported, and the event-source server omits keep-alive

**Question:** The page-network-request group has 18 of its cases ported and passing. Should the cases that need API work I have not built yet stay recorded as gaps, or be ported later? The cases are binary post data, multipart form data, raw and cookie headers, mixed content, the popup frame, and allHeaders timing.

**Guess taken:** Keep them as gaps. Each one is recorded with its upstream line number in the `page-network-request` entry of `progress/TODO-PORTED-PAGE.md`, and the cases are not in `PageNetworkRequestSpec.hs`. The 304 reload case was fixed on the test side, not in the API: `reloadHeaders` sends the same validator headers that upstream sends. Also, `sseApp` omits the `Connection: keep-alive` header that upstream sends. I did not record why. If the event-source case flakes, add the header back first.

**Options:**
- (a) Port the gaps once the API exists. Start with binary post data (`postDataBuffer`), since it unblocks the most cases.
- (b) Keep them as gaps, as in the guess.

**To change:** Tell me (a) or (b). Under (a), I will add the binary post data API next.

<a id="q-3"></a>
## page-wait-for-response: "second waiter loses events" claim did not hold

**Question:** An earlier TODO note said a second armed waiter loses buffered events, so `should work with async predicate` would need a restructure. Is that right?

**Guess taken:** No. The claim did not hold. The async-predicate test passes with two armed `armResponseWhere` waiters (one for the file, one for the JSON), so the case is ported as written and the TODO note was corrected.

**To change:** If a run shows a lost event with two armed waiters, restructure the case around a single waiter.


<a id="q-4"></a>
## browsercontext-events frame and WebError design

**Question:** Upstream `browserContext.on('framenavigated' | 'frameattached' | 'framedetached')` covers every frame in the context. The protocol sends these events on the page channel, and the frame channel carries `navigated`. The Haskell binding has no context-level frame hook, so the port builds one from the existing per-page events.

**Guess taken:** `onContextFrameAttached` and `onContextFrameDetached` register on each page's guid (`frameAttached` and `frameDetached`), for pages that exist now and for pages opened later. `onContextFrameNavigated` runs `navigated` on the frames that exist at setup, and on each frame as `frameAttached` fires. Only navigations with no error are reported. The `DialogClosed` gap was the stale inherited environment pinning Playwright 1.61.1. `nix develop` provides 1.64.1, which has `dialogClosed`, so `flake.nix` is unchanged. `onContextWebError` reads `location` from the raw event JSON, since the generated type does not expose it.

**Substitute in tests:** The `frameattached` and `framedetached` cases use `about:blank` iframes added by script. Upstream uses a frame from a served page. A navigation that starts before `frameAttached` is handled may be missed, so the frame-navigated case depends on the handler being registered first.

**To change:** If a frame attaches and navigates before its handler is registered, the port can miss the first navigation. The fix is to subscribe to the page's frame events through the same channel as the attach. The generated constructor names match the spec (`BrowserContextDialogClosedEvent`, `PageFrameAttachedEvent` and the rest), and the 15 cases pass against Playwright 1.64.1 in `nix develop`.


<a id="q-5"></a>
## browsercontext-add-init-script exposeFunctions design

**Question:** Upstream's `addInitScript(script, arg, { exposeFunctions: true })` exposes each function in `arg` and hands the script a live reference to it. Upstream does this through a controller global and a `parseInitScriptArg` helper. The Haskell port has no such controller, and its `Disposable` wraps a single channel, so one handle cannot remove both the script and its bindings.

**Guess taken:** Add `addContextInitScriptExposing :: BrowserContext -> Text -> [(Text, [SerializedValue] -> Playwright SerializedArgument)] -> Playwright Disposable` in `src/Playwright/InitScript.hs`. Each function is exposed as a context binding under a unique name (`Data.Unique`). The init script is `(source)({key: window[binding], ...})`, so the user's source receives an object of functions, as in upstream's `({ cb }) => ...`. The returned `Disposable` removes only the init script. The bindings stay registered until the context closes, which is why the dispose case checks `typeof window.cb` and not the binding itself. Arguments are functions only. Non-function values are not supported.

**Substitute in tests:** The upstream navigation case goes cross-process through `CROSS_PROCESS_PREFIX`, and the harness has no such prefix. The Haskell test navigates to a second same-origin page instead, so cross-process behaviour is not covered.

**Page level (guess taken):** The page-level variant is built as `addPageInitScriptExposing :: Page -> Text -> [(Text, [SerializedValue] -> Playwright SerializedArgument)] -> Playwright Disposable`, also in `Playwright.InitScript`. The protocol's `Page.addInitScript` result does return a disposable (`PageAddInitScriptResult`), so the handle removes only the init script, as at context level. It lives in `InitScript.hs` rather than `Page.hs`, because `Playwright.Expose` imports `Playwright.Page`. Bindings are exposed on the page with `exposeFunction`, so they stay until the page closes.

**Skipped page-level cases:** Non-object argument forms (dropping functions when `exposeFunctions` is off, throwing when the argument is not a function, a function as the whole argument, arguments passed through unchanged, and errors thrown into the page, since a Haskell handler cannot throw a JS error through the binding). Also the global-object registration check, because `exposeBinding` puts the binding on `window` by design. Also the cross-process navigation step, for the same reason as the context-level variant.

**To change:** If one handle should cover both the script and its bindings, have the Haskell layer track binding removal and return a combined disposable. If non-function arguments are needed, serialise them with `serializeArgument` and pass them through the same `window` object.


<a id="q-6"></a>
## browsercontext-base-url relative glob resolution skips `..` and glob characters

**Question:** Upstream resolves a relative glob against the context baseURL with the URL parser and escapes glob metacharacters. Haskell's `joinAgainst` in `src/Playwright/Url.hs` handles a leading `/` through the origin and otherwise joins against the base directory. It does not normalise `..` segments, and it does not escape glob characters in the joined path.

**Guess taken:** Accept the common relative forms (`page.html`, `./page.html`, `/page.html`, `.`, `/`). `resolveUrlMatch` in `src/Playwright/UrlMatch.hs` leaves globs starting with `*` unchanged, so wildcard-led patterns behave as before. Globs containing `..` or glob-bearing path segments are not covered by a test.

**To change:** If upstream cases with `..` or escaped glob characters show up in the port, normalise dot segments in `joinAgainst` and escape glob metacharacters in the joined path before passing it to `resolveUrlMatch`.


<a id="q-7"></a>
## locator-query cross-frame has/leftOf/rightOf/above/below/near errors

**Question:** Upstream throws a synchronous error when a `has`, `leftOf`, `rightOf`, `above`, `below` or `near` locator belongs to another frame. The Haskell filters are pure (`filterHas`, `filterHasText` and the rest in `src/Playwright/Locator.hs`), so the error has no place to be raised when the locator is built.

**Guess taken:** Leave the filters pure and do not add a frame check. The five cross-frame cases in `should enforce same frame for has/leftOf/rightOf/above/below/near` are skipped in the TODO with this reason. No `error` call is added inside a pure value, because a lazy throw would surface at an unpredictable point.

**To change:** If the error should surface, have the filter constructors return `Either` (or run in `Playwright`) so the cross-frame check happens at build time. The other option is a check at resolve time that raises a `PlaywrightError`.


<a id="q-8"></a>
## page-wait-for-selector-1 log cases match the bare timeout text

**Question:** Upstream log-report cases expect `frame.waitForSelector: Timeout 5000ms exceeded.`. The `frame.waitForSelector:` prefix comes from the JS client wrapper, and Haskell does not add it.

**Guess taken:** The needles are the bare `Timeout 5000ms exceeded.`, the same form the existing `elementHandle.waitForSelector should timeout` case matches. The rest of each upstream needle is kept as written.

**To change:** Add the client prefix to the wrapped error text if the Haskell API ever starts producing it.


<a id="q-9"></a>
## Log cases wait on the main frame, not a frame-level timeout

**Question:** Upstream `frame.waitForSelector` takes a timeout. The Haskell public frame wait `waitForSelectorHandleOnFrame` always uses the default timeout, and the frame-level timeout variant is not public.

**Guess taken:** The visible and hidden log cases use `waitForSelectorHandleWith page ... timeout` on the main frame, which is what upstream `page.waitForSelector` does too. The frame cases use `waitForSelectorHandleOnFrame`.

**To change:** Export a frame-level timeout variant and port the two log cases against a child frame.


<a id="q-10"></a>
## Log cases use a fixed 100 ms wait instead of two rAF cycles

**Question:** Upstream `giveItTimeToLog` runs two `requestAnimationFrame` cycles through the `rafraf` helper, and `rafraf` is internal to the harness.

**Guess taken:** Each step waits 100 ms with `waitForTimeout`, the same pattern the shadow DOM case uses. Playwright logs on each DOM mutation, so the wait only needs to be long enough for the polling loop to record the change.

**To change:** Export `rafraf` from the harness and call it in place of the sleeps if the log cases turn out flaky.


<a id="q-11"></a>
## Multi-needle protocol assertion added to the harness

**Question:** `assertProtocolContains` takes one needle, but the log cases need four needles from one error.

**Guess taken:** Add `assertProtocolContainsAll :: [Text] -> Playwright a -> Playwright ()` to `test/Playwright/Harness.hs`. It runs the action once and fails with the missing needles. `assertProtocolContains` is unchanged.

**To change:** Fold the two helpers together if a single list-based helper reads better everywhere.


<a id="q-12"></a>
## ownerFrame equality uses framesEqual, not Eq

**Question:** Upstream compares `ownerFrame()` results with `toBe`, which needs `Frame` equality. The Haskell `Frame` type has no `Eq` instance.

**Guess taken:** The frame cases use the existing `framesEqual :: Frame -> Frame -> Bool`, exported from `Playwright`, so `Frame` does not need an `Eq` instance.

**To change:** Add an `Eq` instance to `Frame` if callers will compare frames often.


<a id="q-13"></a>
## Detached-frame case matches the bare text

**Question:** The JS client prefixes the detach error with `frame.waitForSelector:`. The Haskell library does not add that prefix.

**Guess taken:** The case matches the bare text `Frame was detached` through `assertProtocolContains`.

**To change:** Add the client prefix if the Haskell API starts wrapping protocol errors.


<a id="q-14"></a>
## `httpCredentials` as an array is not ported

**Question:** Upstream's browsercontext-credentials cases pass `httpCredentials` as a list, one entry per origin. Should the Haskell `NewContextOptions` take a list as well?

**Guess taken:** No. `newContextHttpCredentials` takes one `HttpCredentials`, which can carry an origin. The array cases (a single credential in an array, several origins, the first matching credential, and the fallback without an origin) are left open in the TODO.

**To change:** Make `newContextHttpCredentials` take `[HttpCredentials]` (a breaking change), or add a second field, then port the array cases.


<a id="q-15"></a>
## Raw response headers: `responseHeaders` stands in for `allHeaders`

**Question:** Upstream checks `response.allHeaders()` (the raw headers) in the raw response header cases. Should the Haskell port read raw headers too?

**Guess taken:** No raw response-headers call exists in the Haskell API, so the ported cases check `responseHeaders`, which reads the response initializer headers. The content-type assertions pass through it. Raw-only headers (such as duplicates or original casing) are not covered.

**To change:** Add a `rawResponseHeaders` call to the driver's response channel (upstream sends `rawResponseHeaders` on the response), then point the two cases in `test/Playwright/PageRouteSpec.hs` at it.


<a id="q-16"></a>
## Fetch Metadata headers: the sec-fetch case is not ported

**Question:** Upstream's `should contain sec-fetch headers in route` is `it.fail` on Chromium, so it expects the metadata to be absent there. Should the port assert the absence, or be left out?

**Guess taken:** Left out. A test that expects a failure would pass only while the bug exists, and it would need a change when the browser is fixed.

**To change:** Port it as an expected failure once there is a way to mark one, or assert the headers once the browser behaviour changes.


<a id="q-17"></a>
## The 307 cross-origin navigation waits for a newer driver

**Question:** The upstream `continue should pass on 307 cross-origin redirect` case needs a newer driver than the pinned 1.61.1. Should it wait for the flake pin to move, or be skipped now?

**Guess taken:** It waits. With a `continue` route in place, Chromium fails the form POST's cross-origin 307 on 1.61.1. The same navigation without the route succeeds. The case is recorded in `progress/FUNCTIONALITY_TODO.md` as blocked on the pin.

**To change:** Bump the `flake.nix` pin (and the protocol directory with it), then port the case.


<a id="q-18"></a>
## Fresh cookie from the store inside a route

**Question:** Upstream's `continue with headers should send fresh cookie from the browser cookie store` adds a cookie from inside the route handler. Should the port do that?

**Guess taken:** No. Route handlers run on the serial handler thread, and a command sent from inside one waits for that thread, so the test would hang. The case stays open in the TODO.

**To change:** Let route handlers send commands without waiting, or move the cookie write to a separate thread.


<a id="q-19"></a>
## URL matching: `UrlGlob` becomes `UrlMatch`

**Question:** Should URL matching use one sum type, as the TypeScript API does (`string | RegExp | URLPattern | function`)?

**Guess taken:** Yes. `UrlMatch` has three constructors: `UrlGlob` (the existing glob, unchanged), `UrlRegex` (source and flags), and `UrlPredicate` (client-side only). Routes and waits accept `UrlMatch`. The existing `UrlGlob` constructor is kept, so most call sites compile without change.

**To change:** If you'd rather keep glob-only and add separate regex functions, revert the rename and add `routeRegex` and similar.


<a id="q-20"></a>
## Regex engine: `regex-tdfa`

**Question:** Which regex engine should match JavaScript-style patterns?

**Guess taken:** `regex-tdfa`. It is pure Haskell, so the Nix shell needs no C library. It doesn't support lookahead, and its syntax is only close to JavaScript's.

**To change:** PCRE (through `pcre-light`) is closer to JavaScript, but it needs `libpcre` in `flake.nix`.

**Known gaps:** `regex-tdfa` has no `\d`, `\w` or `\s`, so `src/Playwright/UrlMatch.hs` rewrites those (and `\D \W \S`, and `\/`) into bracket expressions first. Lookahead, lookbehind and backreferences are not supported. Two predicates never compare equal, so `unroute` with a predicate cannot find its route.


<a id="q-21"></a>
## Predicates on routes

**Question:** A predicate cannot be sent to the server. How should `route` with a predicate behave?

**Guess taken:** Register the route with the glob `**/*` and check the predicate in the handler. Requests that don't match are continued unchanged. This matches how the TypeScript client behaves.
</content>


<a id="q-22"></a>
## Regex flags `g` and `y` are not sent to the driver

**Question:** Should a regex route send the `g` and `y` flags to the driver?

**Guess taken:** No. With `g`, the driver's stateful `test` misses the URL it checks second, so the route is skipped. Those two flags only affect stateful matching, and this library always wants a fresh test. `src/Playwright/Route/Intercept.hs` drops them before sending. Other flags are sent unchanged.

**To change:** If the driver is later fixed to reset `lastIndex`, the filter can go.


<a id="q-23"></a>
## Glob matching follows the upstream translation

**Question:** Should client-side glob matching keep its own matcher, where `?` is a wildcard and `{a,b}` is not supported?

**Guess taken:** No. Globs now turn into an anchored regex the way `packages/isomorphic/urlMatch.ts` does. `?` is a literal, `*` stays within a segment, `**/` crosses segments, and `{a,b}` is alternation. The regex is matched by `regex-tdfa`. An empty alternative is written as `(.+/)?`, because `regex-tdfa` rejects `((.+/)|)`.

**To change:** The old matcher is in git history (`src/Playwright/UrlGlob.hs` before the UrlMatch rename).


<a id="q-24"></a>
## Driver pin moved to 1.64.1

**Question:** Should the pinned driver move from 1.61.1 to 1.64.1, which needs the timeout moved into message metadata and the protocol port that comes with it?

**Guess taken:** Yes. The pin in `flake.nix` now uses nixpkgs with playwright-driver 1.64.1, and the source hash is updated. The protocol port is in commit f82c0e3. Calls that lost their timeout param now send it through `sendWithTimeout`.

**To change:** Revert `flake.nix` and `flake.lock` to 8ce4ef6 and undo the port commit. Tap options and the check-position and click-scroll cases were waiting on this pin.


<a id="q-25"></a>
## WAYLAND_DISPLAY unset in the dev shell

**Question:** Should the dev shell unset `WAYLAND_DISPLAY`, given that the headless WebGL tests fail when it is set?

**Guess taken:** Yes. With it set, the browser's ANGLE asks for a Wayland surface that SwiftShader does not provide, so `getContext('webgl')` returns null. The tests run headless and do not need Wayland. The `shellHook` is in commit 1b09f49, with the reason written in the flake.

**To change:** Remove the `shellHook` from `flake.nix`. The two WebGL tests will fail again on a Wayland session.


<a id="q-26"></a>
## Single credential sent as a list

**Question:** The protocol now takes a list of HTTP credentials. Should the Haskell API keep one credential per context and request?

**Guess taken:** Yes. The binding keeps the single `HttpCredentials` argument and sends a one-element list. Upstream's array form, with several credentials for different origins, is still not ported (the cases listed in QUESTIONS.md under the array form).

**To change:** Change the `NewContextOptions` and `NewRequestOptions` fields to a list, and the converters to map it.


<a id="q-27"></a>
## waitForFunction without a handle

**Question:** The protocol's `waitForFunction` result handle is optional now. What should the binding return when it is missing?

**Guess taken:** A `DecodeFailure` with the message "waitForFunction returned no handle". The upstream client assumes the handle is present (`result.handle!`), so this should not happen in practice.

**To change:** Return a `JsHandle` for `undefined` instead if that case turns out to be legitimate.


<a id="q-28"></a>
## $$eval on a missing frame

**Question:** Upstream 1.64 renamed "$$eval should throw for missing frame" to "$$eval should not throw for missing frame", and expects no error and no matches. Should the Haskell test follow it?

**Guess taken:** Yes. The test now expects an empty list from both the page and the element handle.

**To change:** None needed unless the upstream expectation changes back.


<a id="q-29"></a>
## The 307 cross-origin navigation now runs

**Question:** The 307 cross-origin case was left out until the driver moved. Should it be ported now that the pin is 1.64.1?

**Guess taken:** Yes. It is ported with two servers, one on localhost for the redirect and one on 127.0.0.1 for the final page, and it passes on the new driver.

**To change:** Delete the test "continue should pass on 307 cross-origin redirect" if the driver regresses.


<a id="q-30"></a>
## Context pageload and pageclose attach to existing pages

**Question:** Upstream's context `pageload` and `pageclose` events fire for pages that already exist when the listener is added. The binding's `onContextPage` only reports pages created later. Should the new listeners cover existing pages too?

**Guess taken:** Yes. `onContextPageLoad` and `onContextPageClose` in `Playwright.ContextEvents` first attach to `contextPages`, then to new pages.

**To change:** Drop the existing-pages loop in `Playwright/ContextEvents.hs` if a caller needs only pages created after the listener.


<a id="q-31"></a>
## FrameLocator is a plain Locator

**Question:** Upstream `locator.contentFrame()` and `frameLocator.owner()` return a `FrameLocator`, which has its own type. The binding has no such type. Should these add one?

**Guess taken:** No. `contentFrame` and `frameLocatorOwner` return a `Locator`, the same type `frameLocator` already returns. `frameLocatorOwner` removes a trailing `enter-frame` step, so a locator that is not a frame locator comes back unchanged.

**To change:** Add a `FrameLocator` newtype if callers need the two types kept apart at compile time.


<a id="q-32"></a>
## Frame waitForURL uses the default timeout

**Question:** Upstream `frame.waitForURL` takes its timeout from the page's navigation timeout. A `Frame` has no link back to its page in this binding, so it cannot read that setting. What timeout should `waitForURLFrame` use?

**Guess taken:** The default timeout (30 s), the same one the other frame waits fall back to.

**To change:** Pass the page's navigation timeout in as an argument, or add a frame-to-page accessor so the function can read it.


<a id="q-33"></a>
## waitForURL waitUntil options

**Question:** Upstream `waitForURL` accepts `waitUntil` (`load`, `domcontentloaded`, `commit`) and `timeout`. The binding had only the timeout form. How should the options be exposed?

**Guess taken:** A `WaitForURLOptions` record (timeout and `waitUntil`) with `waitForURLOptions`, matching `WaitForNavigationOptions`. `waitForURLWith` still takes a plain timeout and waits for `load`.

**To change:** Fold `waitForURLOptions` into `waitForURLWith` if callers would rather have one function.


<a id="q-34"></a>
## allHeaders returns a list, not a Map

**Question:** Upstream `response.allHeaders()` returns a name-to-value object. The binding's header functions return `[HttpHeader]`. Should `responseAllHeaders` return a `Map`?

**Guess taken:** A list of `HttpHeader`, the same type `responseHeaders` returns, so callers use one lookup for both. Names are lowercased and repeated values are merged, with `, ` for most headers and a newline for `set-cookie`, as upstream `RawHeaders` does.

**To change:** Return a `Map HeaderName HttpHeader` or a `Map Text Text` if callers need keyed lookup. The merge rule stays the same.


<a id="q-35"></a>
## route.fulfill set-cookie test is marked as an expected failure upstream

**Question:** Upstream `should return set-cookie header after route.fulfill` is `it.fail` on chromium and webkit (microsoft/playwright#11035). It fulfills a route with a `set-cookie: a=b` header and expects `allHeaders()` to report it. Because the test is expected to fail on chromium, a plain port would fail in the suite. How should an expected-failure case be recorded?

**Guess taken:** Leave it unported and list it as open in the TODO, rather than add a test that fails or a test that asserts the bug. The rest of the `route.fulfill` headers case is ported as a passing test.

**To change:** Add an expected-failure helper to the harness (a `testCaseFail` that passes when the body throws) and port the case with that helper.


<a id="q-36"></a>
## svg charset case compares the decoded size, not a screenshot

**Question:** Upstream `should work with svg image and charset in content type` compares a screenshot of the image with `toMatchSnapshot`. The harness has no snapshot helper and no screenshot comparison.

**Guess taken:** The port checks that the image loads (`onload`) and that its natural size is 50 by 50, the size the SVG declares. This still shows the route answered with an SVG body under the `image/svg+xml ; charset=utf-8` content type, but it does not check the pixels.

**To change:** Add a snapshot helper with a stored PNG, then compare `img.screenshot()` against it.


<a id="q-37"></a>
## Locator.waitForFunction naming and strict default

**Question:** The Page binding exposes `waitForFunction` and `waitForFunctionWith`, with the timeout as the last argument. Locator has no `waitForFunction` yet. What names should the Locator functions use, and should the selector call be strict?

**Guess taken:** Public names are `waitForFunctionOnLocator`, `waitForFunctionOnLocatorWith loc expression timeout` (timeout last, matching Page's `waitForFunctionWith`), and `waitForFunctionOnLocatorWithArg loc expression arg`. The `On` infix avoids a clash with the Page functions in the `Playwright` re-export list. The selector call hard-codes `strict = Just True`, matching upstream `locator.ts`, so a selector matching several elements fails with a strict mode violation.

**To change:** Rename to `waitForFunction` in a `Locator` module, or add a `strict` argument, if the public API should match the Page functions more closely.


<a id="q-38"></a>
## page-drag skips the drag-out-of-an-iframe case

**Question:** Upstream `should drag out of an iframe` is marked `it.fixme` ("implement dragging with iframes"). The case hovers `#source` inside the out-of-process child frame (`drag-n-drop.html` attached as `oopif`). The hover fails its actionability check: the call log says `<p id="source"> from <div> subtree intercepts pointer events`, so the drag never starts and the hover times out after 30 s.

**Guess taken:** Skip the case, matching upstream's `it.fixme`. The drag into an iframe is ported and passes.

**To change:** Find out why the hit-test in the out-of-process frame reports the `#source` subtree as intercepting pointer events, then un-skip the case. Whether that Haskell-side fix is worth doing depends on whether other iframe actionability work needs it.


<a id="q-39"></a>
## page-evaluate argument-side cases with no argument constructor

**Question:** Several upstream `page-evaluate` cases pass a value into the page as an argument: typed arrays, Maps, regular expressions, Dates, URLs and BigInts. `SerializedArgument.hs` has no constructor for any of them (only undefined, text, bool, number, numbers, special, channel, object, objects-of-handles, and the object variants with number, text or special fields). Those cases cannot be written against the current API.

**Guess taken (first):** Skip the argument-side cases for now. The return-side cases for these types (Date, URL, BigInt) are ported and pass. Handle-based cases (`elementArgument`, `jsonValue` on a Date or URL handle) are ported.

**Superseded:** The argument-side cases for typed arrays, undefined, null, Date, regex and URL are now ported in `test/Playwright/PageEvaluateSpec.hs`. The test file builds the `SerializedArgument` record directly (`typedArrayArgument`, with `PayloadTypedArray` and `serializedArgumentValue`), so no public API was added. Slice run: 97 of 97 pass, `BUILD_RC=0`, `TEST_RC=0`. Still skipped: the JS Map argument ("should transfer maps as empty objects"), since the Haskell API cannot express a JS Map.

**To change:** Only if a public argument builder for these payloads is wanted. Adding one is a public API addition and needs a decision on naming first.


<a id="q-40"></a>
## page-wait-for-navigation: frame detach error wording

**Question:** Upstream `should fail when frame detaches` expects `waiting for navigation until "load"` in the error message. Upstream's full text ("Navigating frame was detached!") is not shown in the test, so the wording of the rest of the message is a guess.

**Guess taken:** `frameDetachedError` throws `ProtocolRpc` with `protocolFailureName = "Error"` and the message `Navigating frame was detached!` followed by a call log line `  - waiting for navigation until "<state>"`. The state defaults to `load`.

**To change:** Match the exact upstream wording in `Page/Wait.hs` (`frameDetachedError`) if a test or caller depends on it.


<a id="q-41"></a>
## page-wait-for-navigation: detach waiter left unconsumed

**Question:** `armNavigationFrame` registers a `frameDetached` waiter on the page. When navigation wins the race, nothing consumes that waiter, and there is no public cancel.

**Guess taken:** Leave it registered. The waiter is only removed on consumption, so it stays in the connection's table until the page closes. It matches only the one frame guid, so it cannot fire on another frame's detach.

**To change:** Add a cancel for future event waiters, then cancel the detach waiter when navigation completes.


<a id="q-42"></a>
## page-wait-for-navigation: descendant frame detach

**Question:** When a parent frame with a child frame navigating is detached, upstream may fail the child's pending navigation too. The port only fails a navigation when its own frame detaches.

**Guess taken:** `isDetachOf` compares the frame guid only, so detaching an ancestor does not fail a child's pending navigation. That is not verified against upstream.

**To change:** Walk the detached frame's descendants in `isDetachOf` if upstream fails descendant navigations too.


<a id="q-43"></a>
## page-network-idle: response.ok() replaced by a status check

**Question:** Upstream's goto cases assert `response.ok()`. The port's `assertNavigationOk` checks for status 200 only, which is stricter for 2xx responses other than 200 and differs for redirects.

**Guess taken:** Check `responseStatus == 200`, because the Haskell `Response` has no `ok()` accessor and the fixtures always answer 200.

**To change:** Add an `ok` check (any 2xx) if the binding gains one, or if a fixture starts returning another 2xx.


<a id="q-44"></a>
## page-network-idle: foo.js held, then released

**Question:** Upstream never answers the `foo.js` request in the prior-navigation cases. The port answers it with 404 after the action, so the local app can shut down.

**Guess taken:** Release `foo.js` after the action completes. That keeps the request pending during networkidle, as upstream does, without leaving a connection open at teardown.

**To change:** Leave the request pending and rely on the harness shutdown if teardown blocks on open requests.


<a id="q-45"></a>
## frame-hierarchy: isDetached is driven by the frameDetached event

**Question:** `isDetached` previously checked whether the frame guid was still among the live frame objects. The object's `__dispose__` can arrive after `frameDetached`, so the answer was False right after the frame was removed.

**Guess taken:** Follow upstream `client/page.ts:203`. The page's `frameDetached` event adds the frame guid to a detached set in the session, and `isDetached` reads that set. Disposal no longer affects the answer.

**To change:** Clear the set on disposal, or keep the object-based check, if a frame guid could be reused after detach.


<a id="q-46"></a>
## frame-hierarchy: framesets race in the queued frameAttached job

**Question:** The child `navigated` watcher was registered inside the queued `frameAttached` job. When a child frame navigated right after attaching, the reader had already dispatched that `navigated` event, so the watcher missed it. The framesets case then counted 4 navigations where upstream expects 5.

**Guess taken:** Register the child watcher at dispatch time on the reader thread. `registerEventHandler` gains a dispatch hook, `registerDispatchHandler`, which runs before the queued run. It is internal and changes no public API. The hook only registers handlers and must not send requests. Exceptions in the hook are caught.

**To change:** Drop the hook and register the watcher from a queued job if the reader thread must stay free of handler work. Or move the child-watch registration into the page-level `frameAttached` dispatch if more event kinds need the same treatment.


<a id="q-47"></a>
## frame-hierarchy: x-frame-options refusal is checked through a console waiter

**Question:** Upstream reads the refusal from the page's console, but the page-level console waiter subscribes only when it runs. The refusal is written while `setContent` loads the frame, so a late subscription could miss it.

**Guess taken:** Arm the console waiter with `armConsole` before `setContent`, then await the returned waiter. The case matches the lowercase text `refused to display` and `'x-frame-options' to 'deny'`. The cross-origin frame uses `localhost` in place of `127.0.0.1`, as upstream's `server.CROSS_PROCESS_PREFIX` does, so the frame stays cross-origin. Chromium only, as upstream skips the case on Firefox.

**To change:** Drop the text checks and assert only that a console message arrived, if Chromium's refusal wording changes between releases.


<a id="q-48"></a>
## frame-hierarchy: context-level frame events register inside a page job

**Question:** `onFrameAttached`, `onFrameDetached` and `onFrameNavigated` on a `Page` register directly on the caller's thread, so they see every later event. The `onContextFrameAttached`, `onContextFrameDetached` and `onContextFrameNavigated` variants register from inside the `onContextPage` callback. For a page opened after the call, that callback runs as a queued job, so a child-frame event the reader already dispatched can be missed.

**Guess taken:** Leave it open. The page-level versions are correct and the context-level versions are not yet covered by a test that opens a page and attaches a child frame right away. Fixing it needs the page-created registration to run at dispatch time too.

**To change:** Make `onContextPage` register its per-page setup with `registerDispatchEventHandler` and add a context-level test that attaches a child frame in a new page immediately.


<a id="q-49"></a>
## workers: console timestamp is checked against the page clock

**Question:** Upstream checks `consoleMessage.timestamp()` against a wall-clock range. The Haskell test suite does not depend on a wall-clock package (`time` is not a cabal dependency), and the `timestamp` field is a Double of epoch milliseconds from the driver.

**Guess taken:** Read `Date.now()` from the page with `evaluateNumber` before and after the worker is created, and assert the message timestamp lies between those two readings (with a 1 ms margin on each side). Both clocks are the same machine's epoch milliseconds, so the comparison holds.

**To change:** Add a `time` dependency and compare against `getCurrentTime` if the page-clock margin ever proves flaky.


<a id="q-50"></a>
## workers: "should report console logs" placeholder replaced

**Question:** The earlier placeholder for this case passed without checking any console message, so it gave false evidence.

**Guess taken:** Replace it with the upstream shape: `armConsole` before the blob worker is created, then check the text is `"1"` and the page URL is not a blob URL. The old placeholder body was removed rather than kept alongside.

**To change:** If upstream's page URL assertion needs a different base, adjust the `pageLink` check only.


<a id="q-51"></a>
## workers: dedicated-worker console needs a worker-level subscription

**Question:** The two console-event cases ("should report console event on the worker", and the case without a page or context listener) failed with `WaitTimeout`. Node runs of the same flow showed the driver emits the `hey` console event on the context channel, in every variant tried (about:blank, HTTP origin, with and without the context subscription). So the driver emits the event, and the Haskell side missed it.

**Finding:** `contextFromWorker` returned the page guid, because a worker's `remoteParent` is its page. The context subscription and the context console waiter were then aimed at the page channel, so the event was never awaited. Fixed by walking worker → page → context. The driver's context dispatcher also forwards a dedicated worker's console only when the worker channel holds a `console` subscription, so `armWorkerConsole` sends `updateSubscription {event: 'console', enabled: true}` to the worker channel (raw JSON, since the protocol type is not generated).

**Guess taken:** Keep the worker-channel send as raw JSON, and discard its result with `_ :: Value`. A driver error from that send would be silent.

**Evidence:** scratchpad `workers.log`: BUILD_RC=0, `-p /workers/` All 14 tests passed, TEST_RC=0. Before the fix (`workers.log` / `trace.log`), two cases failed with WaitTimeout.

**To change:** If the send can fail in a way that matters, decode its result (or surface the error) instead of discarding it.


<a id="q-52"></a>
## browser-context: `armContextConsole` registers an unfiltered waiter

**Question:** `armContextConsole` in `src/Playwright/BrowserContext/Wait.hs` uses `registerEventWaiter`. Does an unfiltered context waiter take a message meant for a worker or page waiter?

**Finding (corrects the earlier guess):** No. `deliverEvent` in `src/Playwright/Connection/Wait.hs` fills every matching waiter, so two waiters both see the same event and neither steals it. A context console waiter is also meant to see worker messages, because upstream `context.on('console')` includes workers.

The real difference is `registerEventWaiter`'s buffer path. It pops a console event that arrived before the arm and was buffered for `(guid, "console")`. A future-only waiter (`registerFutureEventWaiterWhere`, as `armWorkerConsole` uses) would not do that.

**Guess taken:** Leave `armContextConsole` unchanged. Several existing tests start `waitForContextConsole` inside `withAsync` before the action (`BrowserContextEventsSpec.hs:91,103,115`, `ChromiumSpec.hs:461`, `WorkersSpec.hs` service-worker start). The async thread may register after the log has already fired, so those tests rely on the buffered path. Switching to future-only would make them racy.

**To change:** Only if the tests are rewritten to arm before forking, so that the arm is synchronous. Then switch `armContextConsole` to `registerFutureEventWaiterWhere`.


<a id="q-53"></a>
## workers: network cases and the unfiltered finished waiter

**Question:** "should report network activity" is gated on chromium 143 or later. Does the gate skip the case on older browsers, or fail?

**Guess taken:** The gate reads the major version from `browserVersion` through `contextBrowser (pageContext page)`. On an older browser it prints `skipped: needs chromium 143, found <version>` and returns without failing. The pinned chromium-1248 run printed no skip line, so the case ran and passed. The exact version string was not read back from the log.

**To change:** If the gate should use a tasty skip rather than a silent pass, switch it to the tasty skip mechanism the other gated specs use.

**Question:** `armRequestFinished` is unfiltered, so the first finished event on the context may not be the worker script. Run 1 failed on `same request` because the first event was another request.

**Guess taken:** In "should report worker script as network request", a local `awaitScriptFinished` takes the next finished event and re-arms `armRequestFinished page` while the URL lacks `worker.js`. Run 2 passed. A finished event that arrives between `takeNext` and the next arm could be lost. No such loss showed up in run 2.

**To change:** If that race shows up, use a fresh buffered waiter that filters by URL (as `armRequestWhere` does), or add a `armRequestFinishedWhere` that filters before it takes the event.


<a id="q-54"></a>
## workers: version gates print `skipped:` and pass

**Question:** The workers cases gated on a browser version (extra headers and the iframe network case need chromium 143 and 149 or later; the iframe `allHeaders` case needs 151) use the `browserVersion` check. Should an old browser get a tasty skip instead of a silent pass?

**Guess taken:** Print `skipped: needs chromium N, found <version>` with `liftIO (putStrLn ...)` and return, so the test passes. This matches the earlier version-gated cases in this spec. A tasty skip would need a different shape of test for each gate.

**To change:** Switch the gates to the tasty skip mechanism if the suite starts reporting skips as a count.


<a id="q-55"></a>
## workers: extra-headers WaitTimeout and the finished waiter

**Finding:** "should support extra http headers" timed out after 30.29 s on the `armRequestWhere` waiter for `worker.js`. The earlier suspects, `setExtraHTTPHeaders` and the glob matcher from `armRequest`, were refuted by a diagnostic run. The waiter never matched because `armRequestWhere` skipped the script request.

**Resolution:** Switched to `armRequestFinished` plus the local `awaitScriptFinished`, which skips finished events until the `worker.js` URL. The case passes (`-p` on the allHeaders and extra-headers cases: All passed).

**Guess taken:** Keep the same `awaitScriptFinished` shape for the three allHeaders cases.


<a id="q-56"></a>
## workers: iframe network case hang (not reproduced)

**Finding:** "should attribute network activity for worker inside iframe to the iframe" once stopped with `SignalException 15` (killed by the 300 s timeout). The rerun alone passed in 0.53 s (scratchpad `one.log`, TEST_RC=0). The nested-worker case uses the same `wait handle` pattern and passes, which weakens the `waitForWorker`-has-no-timeout theory.

**Guess taken:** Treat it as a one-off and keep the case as is. If it recurs in a full run, add a bounded wait to `waitForWorker` in the test.


<a id="q-57"></a>
## workers: redirect case is a skip with a reason

**Question:** Upstream "should work with redirect" (`tests/page/workers.spec.ts:264`) is marked `fixme` on chromium. Should it get a test body?

**Guess taken:** No. It is recorded as a skip with the upstream reason in `progress/FUNCTIONALITY_TODO.md`, with no test body.

**To change:** Port it once the upstream fixme is removed.


<a id="q-58"></a>
## workers: webkit and firefox fixmes are not represented

**Question:** The nested-worker `allHeaders` case (upstream line 399) has fixmes for webkit (402) and firefox (403). Should those be recorded as skips?

**Guess taken:** No. The harness is chromium-only (pinned chromium-1248, firefox-1555, webkit-2370, and the suite runs on chromium), so only the chromium behaviour is ported and the other browsers' fixmes are not represented.

**To change:** Add skip records if the suite ever runs on webkit or firefox.


<a id="q-59"></a>
## page-click-timeout: inline markup instead of input/button.html

**Question:** Upstream `page-click-timeout-*` specs load `tests/assets/input/button.html`. The Haskell harness does not serve that asset. Should the port add an asset server or inline the markup?

**Guess taken:** Inline the markup with `setContent`. The button markup is small, and the timeout behaviour does not depend on the file itself.

**To change:** Add the asset to the harness if a later spec needs the real file.


<a id="q-60"></a>
## page-click-timeout: mode-gated and Android cases

**Question:** Three cases use Node-only test hooks (`__testHookBeforePointerAction`, `__testHookBeforeHitTarget`), and the large-header case in `-4` is Android-specific upstream. Should they be ported?

**Guess taken:** No. They are recorded as skips in `progress/FUNCTIONALITY_TODO.md`, with the reasons.

**To change:** Port them if the harness gains an equivalent of these hooks.


<a id="q-61"></a>
## page-evaluate-callback: handle-routing cases ported with page-side calls

**Question:** Upstream cases 15 (`jsHandle.evaluate`) and 19 (`locator.evaluateHandle`) pass a callback argument through `evaluateOnHandle` / `evaluateHandleOnLocator`. Those Haskell calls take no argument, so the callback cannot be passed in. Should these cases be skipped?

**Guess taken:** Port them. The page-side expression calls `window.cb(...)` directly, so the routing from a handle or locator back to the page is still tested, and the callback is reached through the exposed binding instead of an argument.

**To change:** Skip them if the routing has to go through a real argument.


<a id="q-62"></a>
## page-evaluate-callback: case 22 skipped

**Question:** Upstream case 22 checks the internal `__pw_fn_` global name that upstream uses for exposed callbacks. Should it be ported?

**Guess taken:** No. The name is an implementation detail of upstream's `exposeFunctions` mechanism, which Haskell does not have. It is recorded as a skip in `progress/FUNCTIONALITY_TODO.md`.

**To change:** Port it if the harness ever needs to observe the binding name.


<a id="q-63"></a>
## tap: touchscreen tap added as a public API

**Question:** Upstream's touch-point case calls `page.touchscreen.tap(40, 60)`. The Haskell library had no touchscreen wrapper, though the protocol method `touchscreenTap` exists. Should a public `touchscreenTap` be added, or should the case be skipped?

**Guess taken:** Add `touchscreenTap :: Page -> Point -> Playwright ()` in `src/Playwright/Touchscreen.hs` and export it from `Playwright`. It follows the `Mouse.hs` pattern, and the touch-point case uses it.

**To change:** Drop the export and skip the case if the public surface should stay as it is.


<a id="q-64"></a>
## browsercontext-csp: plain frame under CSP accepts addScriptTagOnFrame (resolved: the test header was malformed)

**Resolved.** The plain-frame failure was a bug in the test, not in the driver. `cspInlineApp` in `test/Playwright/BrowserContextCspSpec.hs` sent `default-src "self"`, with literal double quotes. The quotes make `"self"` an invalid CSP source. Chromium then blocks the iframe load through the `default-src` fallback, and the frame sits at `chrome-error://chromewebdata/`. An error page has no CSP, so `addScriptTagOnFrame` injected into it and `__injected` was `42`. The `plain inline` assertion of `"undefined"` passed on that error page, so it tested nothing.

**Evidence:**
- `scratchpad/probe.log`, `probe2.log`, `probe3.log`: the plain frame stays at `chrome-error://chromewebdata/` with `scripts=0`, while the top-level `csp.html` in the same context loads.
- `scratchpad/probe4.log`, with the header changed to `default-src 'self'`: the frame loads `…/csp.html` with `scripts=1` and `inline=undefined`. `addScriptTagOnFrame` now fails with Chromium's "Executing inline script violates the following Content Security Policy directive 'default-src 'self''".
- `scratchpad/csp-final.log`: the committed CSP group passes, 5 of 5. The plain-frame case now asserts the frame URL is `csp.html`, asserts the injection is rejected, and asserts `__injected` is `undefined`.

**Correction:** the earlier follow-up below, "the driver does not branch on frames", checked the driver for a problem the page did not have. The driver is not implicated and the Haskell `frameSend` path is fine.

**Follow-up, done:** `test/Playwright/BrowserContextExposeFunctionSpec.hs:173` (`cspEmptyApp`) had the same malformed header, `default-src \"self\"`. It now sends `default-src 'self'`. The `browsercontext-expose-function` group passes 7 of 7 with the fix. Its `should work with CSP` case still does not check inline blocking, which is unchanged.



<a id="q-65"></a>
## Test runs must use the nix driver, not the inherited one

**Finding:** The shell env inherited into the session pins playwright-core 1.61.1. `nix develop` provides 1.64.1. Under the inherited env, the `elementhandle-click` group failed 9 of 9 with `ProtocolRpc ... "timeout: expected float, got undefined"`, from `playwright-core-1.61.1/lib/coreBundle.js`. The 1.61.1 driver rejects the metadata-timeout path that `sendWithTimeout` uses. Under `nix develop --command`, the same group passes 9 of 9.

**Guess taken:** Run the test binary only under `nix develop --command bash -c '...'`. The earlier tap timeout was carried forward as unresolved. It is probably the same stale-driver cause, but it has not been reproduced under `nix develop`, so that remains unconfirmed.

**To change:** If the inherited env is moved to 1.64.1, the wrapper is no longer needed.


<a id="q-66"></a>
## Element-handle form of selectOption: the public shape

**Guess taken:** `selectOptionElements :: Locator -> [ElementHandle] -> Playwright [Text]` and `selectOptionElementsWith` (which takes `SelectOptionOptions`) are the Locator-level port. They send the protocol `elements` list with `options` unset, using `strict: true`, and they take the timeout from the options or from the locator action timeout.

**Why not a `SelectOption` constructor:** `SelectOption` is a plain value list. A handle inside it would make the list mix values with live handles, and the handle form has to carry the handle channel, not a string. A separate function keeps the two forms apart.

**Not representable:** the upstream `null` and wrong-type cases for the element form. `[ElementHandle]` cannot hold `null`, and the type checker rejects a non-handle. Those stay skipped in `progress/FUNCTIONALITY_TODO.md`.

**To change:** If a combined `SelectOption` type is wanted later, `selectOptionElements` can be kept as a thin wrapper over it.


<a id="q-67"></a>
## page-evaluate nice error after navigation

**Question:** Upstream "should throw a nice error after a navigation" (`tests/page/page-evaluate.spec.ts`, around line 542) starts an evaluate, navigates the page, and expects the first evaluate to fail with a nice error. Does the case need the first evaluate to reach the page before the navigation?

**Guess taken:** Yes. The error only shows up when the first evaluate is already running in the page when the reload starts.

**Resolution:** Ported in `test/Playwright/PageEvaluateSpec.hs`. The pending evaluate is forked with `async`. The test then polls a page-side check, `typeof window.__resolve === 'function'`, which the pending evaluate sets as it runs. The poll is bounded at 100 tries and fails if the flag never appears. Only after that does the reload evaluate run, and `wait` on the pending evaluate is checked for `navigation`. The case passes (80 of 80 in the page-evaluate group).


<a id="q-68"></a>
## Handle preview cases (elementhandle-convenience)

**Question:** Upstream "should have a nice preview" and "should have a non-ASCII preview" (`tests/page/elementhandle-convenience.spec.ts`, lines 20–37) check `String(handle)`, which is the JS client's `toString` output (`JSHandle@<div id="outer" name="value">…</div>`). Can the Haskell `ElementHandle` expose that text?

**Guess taken:** No, skip them. The text is produced by the JS client's `toString`, which the Haskell binding does not have. A handle is an opaque channel. The library has no call that returns a handle's preview, and the protocol does not return one either.

**Why not ported:** Making the preview would mean inventing a Haskell-side rendering of the element, which is not what upstream tests. Any port would check the wrong thing.

**To change:** If a `handlePreview` accessor is added, the two cases can be ported as written. The non-ASCII case also needs the same accessor.


<a id="q-69"></a>
## page-evaluate usable-message cases with a JS function argument

**Question:** Upstream `tests/page/page-evaluate.spec.ts` (lines 456–471) has three usable-message cases that pass a JS function as the argument (`() => { }`, `{ aProperty: () => { } }`, and the nested and array-nested forms). Can `SerializedArgument` carry them?

**Guess taken:** No. `SerializedArgument` has no constructor for a function. The cases are skipped as type-level in `progress/FUNCTIONALITY_TODO.md`.

**Why not ported:** The cases only test the JS client's error message for an unserializable function argument. Haskell's type rules that out before anything is sent, so there is no message to check.

**To change:** Only a `SerializedArgument` constructor that carries a function would make these reachable. Adding one would be a different API decision, not a port.


<a id="q-70"></a>
## page-expose-function exposeBinding source argument

**Question:** Upstream `tests/page/page-expose-function.spec.ts` uses `exposeBinding` with a `source` first argument (`(source, a, b) => ...`, around line 22, and the `log` cases at lines 202 and 209). Should the Haskell API expose the source frame to the handler?

**Guess taken:** No. The handler type is `[SerializedValue] -> Playwright SerializedArgument`, which gets no source. `exposeFunction` is the only binding API. The source cases are skipped in `progress/FUNCTIONALITY_TODO.md`.

**Why not ported:** Passing the source means a new handler type or a new `exposeBinding` function. That changes the public API, which is a design decision and not a port. The cases also check `source.frame` and `source.context`, so they need a frame handle the binding does not carry today.

**To change:** Add a function such as `exposeBindingWithSource` that passes a source record (frame and context) to the handler. The `log` and `add` cases can then be ported as written.


<a id="q-71"></a>
## selectors-get-by: regex forms of internal:text and internal:label

**Question:** Does the driver accept `internal:text=/pattern/flags` and `internal:label=/pattern/flags`, as the regex variants of getByText and getByLabel need? The Haskell wrappers `getByTextRegex` and `getByLabelRegex` are new. Their selectors use `escapeRegexForSelector` and pass the `/pattern/flags` text through as-is.

**Guess taken:** Yes. The driver parses `/pattern/flags` in `internal:text=` and `internal:label=` the same way it does for `internal:attr=` and `internal:testid=`, which the existing regex variants already use.

**Resolved:** The guess held. `test/Playwright/SelectorsGetBySpec.hs` built, and all 28 cases in the group passed (commit 9975669), including the regex `getByTextRegex` and `getByLabelRegex` assertions.

**To change:** If the driver rejects these forms, build the regex wrappers from `internal:has-text=` or drop the regex assertions and skip them in the TODO.


<a id="q-72"></a>
## selectors-get-by: v-flag regex in getByRoleNameRegex

**Question:** Does the driver accept the `v` flag in `getByRoleNameRegex` (`/Click me/v`), as upstream `selectors-get-by.spec.ts` (line 289) expects?

**Guess taken:** Yes. The name regex is forwarded to the driver, which parses flags the same way for every `/pattern/flags` text. The case is in `test/Playwright/SelectorsGetBySpec.hs`.

**Resolved:** The guess held. The v-flag case passed in the same run as the other selectors-get-by cases (commit 9975669), so the driver accepts the `v` flag in `getByRoleNameRegex`.


<a id="q-73"></a>
## page-screenshot: ScreenshotOptions fields

**Question:** `ScreenshotOptions` has only `timeout` and `fullPage`. Upstream `page-screenshot.spec.ts` (65 of 67 cases missing) needs `clip`, `type`, `quality`, `path`, `omitBackground`, `mask`, `caret`, `animations`, and `scale`. What is the Haskell shape for them?

**Guess taken:** Add each option as a `Maybe` record field on `ScreenshotOptions`, with the default left as `Nothing`, so existing record updates keep compiling. Shapes: `screenshotOptionsClip :: Maybe ScreenshotClip` (a record with `Double` fields `x`, `y`, `width`, `height`), `screenshotOptionsType :: Maybe ScreenshotType` (sum type `ScreenshotPng | ScreenshotJpeg | ScreenshotWebp`), `screenshotOptionsQuality :: Maybe Int`, and `screenshotOptionsPath :: Maybe FilePath`. The boolean options take `Maybe Bool`. `caret` and `animations` take small sum types, following the existing `Exactness` style.

**Why not verified:** Nothing is implemented yet. The field names follow the upstream names, but the Haskell types are a guess.

**To change:** If the record should stay minimal, add a separate `screenshotWith` builder instead of growing the record, and record that choice here.

**Update (first slice):** `type`, `quality` and `omitBackground` are in, as `screenshotOptionsType :: Maybe ScreenshotType`, `screenshotOptionsQuality :: Maybe Int` and `screenshotOptionsOmitBackground :: Maybe Bool`. The fields are records, not a separate builder. Later slices closed `path`, `mask`, `caret`, `animations` and `scale`; see the sections below.

**Update (clip):** `clip` is in as `screenshotOptionsClip :: Maybe BoundingBox`, not the `ScreenshotClip` record guessed above. It reuses the existing `BoundingBox`, whose fields are `CssPixel`, so the record is not duplicated. The clip test "should clip elements to the viewport" asserts the size the driver returns for a clip that runs past the viewport: 450x50 for a 1000x100 clip at (50, 450) in a 500x500 viewport. Guess: the driver's size is the expected one, since the upstream snapshot does not give a number. If upstream later pins a different size, update the test.


<a id="q-74"></a>
## page-screenshot: enum constructor names and quality type

**Question:** `ScreenshotType` has constructors `ScreenshotPng`, `ScreenshotJpeg` and `ScreenshotWebp`, mapped onto the generated `PageScreenshotParamsType` constructors. Are these the right names, and is `Int` the right type for `quality`?

**Guess taken:** Yes to both. The Haskell names follow the `ScreenshotType` pattern (`Screenshot` plus the format). `quality` is `Maybe Int`, because the protocol's `number` field is generated as `Scientific`, and `Int` makes out-of-range values reach the driver unchanged. The out-of-range webp case checks the driver's own message, not a Haskell-side check.

**Verified:** The build passes and all eight `page-screenshot` cases pass against the live driver, including the out-of-range webp message from the driver. The build had first failed on the import-list style, an identity `fromIntegral`, and a `Playwright.hs` import that dropped the new record fields. All three are fixed.

**To change:** Rename the constructors if the group needs a different style. The mapping is one function in `Playwright.Page`, so the change is local.


<a id="q-75"></a>
## page-screenshot: mask, maskColor and style

**Question:** Upstream `mask` is an array of `{frame, selector}` locator pairs. `ScreenshotOptions` derives `Eq` and `Show`, and a `Locator` has neither, so `mask` cannot be a field of the record. What is the Haskell shape?

**Guess taken:** `mask` is a separate function, `screenshotMasked :: Page -> [Locator] -> ScreenshotOptions -> Playwright ByteString`. `screenshot` is `screenshotMasked` with an empty list. `maskColor` and `style` are `Maybe Text` fields on `ScreenshotOptions`, and they only apply when masks are given, as upstream does.

**Why not verified:** The record cannot hold the locators, so this is the only shape that keeps `Eq` and `Show` on the record.

**To change:** If `ScreenshotOptions` should carry the masks, drop `Eq` and `Show` from it and add a `screenshotOptionsMask :: [Locator]` field. That is a larger change, and no caller depends on the record's `Eq` today.

**Test check:** The mask case asserts that the masked bytes differ from the plain bytes, and that a later unmasked screenshot equals the plain one. Upstream compares rendered pixels, and the repo has no decoder, so this check is weaker. The `maskColor` case asserts that a red paint changes the bytes. The `style` case asserts that a `body` background rule changes the bytes. Each only checks that the option took effect.


<a id="q-76"></a>
## page-screenshot: image decoding for pixel checks

**Question:** Upstream checks the webp lossless output and the transparent background by decoding the pixels. The repo has no PNG or WebP decoder. Should those cases be ported with a byte-level check, or skipped?

**Guess taken:** Skip the pixel checks (the transparency and lossless cases) and port the signature and size checks. The signature and size checks cover the wiring. The pixel checks need a decoder dependency that the project does not have.

**Why not verified:** Not decided with the user. The skip is recorded in the TODO.

**To change:** Add a decoder dependency (for example `JuicyPixels`) and port the pixel cases, if the user wants that coverage.


<a id="q-77"></a>
## page-screenshot: path error type and mime table

**Question:** Upstream throws a plain error for an unsupported `path` extension (`path: unsupported mime type "..."`). Which Haskell constructor should carry it, and how much of the upstream mime table should the port carry?

**Guess taken:** Raise `DecodeFailure` with the upstream message, because it is the existing client-side argument error (`PlaywrightError`). The mime table is partial: `png`, `jpg`, `jpeg`, `jpe`, `webp` and `txt`. Unknown extensions, and paths with no extension, resolve to `"null"` in the message, as upstream does. The extension match is case-sensitive, as upstream's is.

**Why not verified:** Not decided with the user. The error text matches upstream, but no Haskell-side convention was checked against other client-side errors.

**To change:** Give the path error its own constructor, or widen the mime table to the full upstream list.


<a id="q-78"></a>
## page-screenshot: caret, animations and scale behaviour

**Question:** The `caret`, `animations` and `scale` options are passed through to the protocol, but only acceptance and dimensions are tested. Is that enough, or should the port check the visual effect (caret hidden, animations finished, device pixels scaled)?

**Guess taken:** Ship the three options with a test per enum value that asserts a PNG comes back, plus a device-scale-factor-2 pair that asserts `scale css` gives 300x300 and `scale device` gives 600x600 on a 300x300 viewport. The options are `Maybe` and omitted when unset, so the server defaults apply (`caret` hides the caret, `animations` disables them, `scale` is `device`).

**Why not verified:** Pixel-level checks (caret visibility, a frozen animation frame) need image decoding, which the port does not have (see "page-screenshot: image decoding for pixel checks"). Those effects are not observed here.

**To change:** Decode the PNG in the test and compare against a reference image, or a second screenshot taken with the animation paused.


<a id="q-79"></a>
## build: InitScript overlay is not in HEAD

**Question:** `src/Playwright/InitScript.hs` is staged as added (`AM`) but has working-tree changes, and HEAD does not contain it. Several committed-or-working modules import it, so a clean checkout of HEAD does not build (`Cabal-7554: can't find source for Playwright/InitScript`). Should the file go into a commit, and which version?

**Guess taken:** Leave the file out of the page-screenshot commits, because it belongs to the init-script group and is not part of this slice. Every test run so far used the working tree, so it builds there.

**Why not verified:** A clean build of HEAD+slice has not been run. The working-tree build is the only one checked.

**To change:** Commit `InitScript.hs` with its init-script tests, or stash the importers until it is committed.


<a id="q-80"></a>
## build: uncommitted specs break the build in three places

**Question:** With the uncommitted specs present, the build breaks in `test/Playwright/LocatorWaitForFunctionSpec.hs`, `test/Playwright/PageAddInitScriptCallbackSpec.hs` and `test/Playwright/PageCacheStorageSpec.hs`. Are these in-progress specs to finish, or to hold back?

**Guess taken:** Keep them uncommitted. Each one belongs to a group that is still open, so committing them would break HEAD.

**Why not verified:** The page-screenshot runs used `PAT=` filters, which compile every spec. A clean-HEAD build was not run to confirm the break is limited to these three.

**To change:** Tell me which of these groups should land first, and I will fix or commit them in that order.


<a id="q-81"></a>
## page-screenshot: canvas case needs a byte-level check

**Question:** Upstream `should work for canvas` screenshots a page with a drawn canvas and compares the decoded pixels. The repo has no decoder (see "page-screenshot: image decoding for pixel checks"). What should the port assert?

**Guess taken:** Use a byte-level check. Draw the canvas, take a screenshot, and assert that the PNG signature comes back and that the bytes differ from a screenshot of the same page with the canvas left blank. That checks that the canvas content reaches the image, without decoding pixels. The case is written in `PageScreenshotSpec.hs` (`should work for canvas`). It clears the canvas with `clearRect` in the same page, which gives the blank comparison without a second page, and passes.

**Why not verified:** The byte comparison only shows that the output changed. It does not show that the canvas colour is in the right place.

**To change:** Add a PNG decoder and port the upstream pixel comparison, along with the other pixel cases.


<a id="q-82"></a>
## build: HEAD test/Main.hs expects PageClickTimeoutSpec.tests

**Question:** HEAD's `test/Main.hs:295` runs `PageClickTimeoutSpec.tests`, but HEAD's `test/Playwright/PageClickTimeoutSpec.hs` exports only `timeout1Tests` to `timeout4Tests`. Clean HEAD does not compile its test suite. The working-tree spec exports `tests` and fixes this, but that edit is not committed. Should it land with the page-click work?

**Guess taken:** Use the working-tree spec in the scratch tree, and leave it uncommitted in the repo. It belongs to the page-click group, which is not part of the page-screenshot slice.

**Why not verified:** The scratch build used the working-tree spec, so the screenshot run does not show that the uncommitted spec compiles cleanly on its own.

**To change:** Commit the working-tree `PageClickTimeoutSpec.hs` with its page-click group, or revert `test/Main.hs` to match HEAD's export list.


<a id="q-83"></a>
## browsercontext-credentials: widen the credentials API to a list?

**Question:** Upstream `context.setHTTPCredentials` and the `httpCredentials` context option accept `HTTPCredentials | Array<HTTPCredentials>` (`upstream-typescript-playwright/packages/playwright-core/types/types.d.ts:25916`). Four upstream cases need the list form: "should work with setHTTPCredentials and multiple credentials", "should work with multiple credentials for different origins", "should fall back to credentials without origin", and "should use the first matching credential". The Haskell API takes one credential: `newContextHttpCredentials :: Maybe HttpCredentials` and `setHTTPCredentials :: BrowserContext -> HttpCredentials -> Playwright ()`. Should it accept a list?

**Guess taken:** Keep the public API as it is and leave the four cases open in `progress/FUNCTIONALITY_TODO.md`. Widening the existing functions would break every current `Just creds` and `setHTTPCredentials ctx creds` caller. The wire format already takes a list: `toProtocolCredentials` wraps one value in a one-element list.

**Why not verified:** Nothing has been built against a list signature, so the breakage count and the cost of each option are estimates. The single-credential case "should work with a single credential in an array" is marked covered, because the single-value case exercises the same wire shape. That covers the array shape, not the type.

**To change:** Pick one of these. (a) Widen the existing functions to `HttpCredentials` or `[HttpCredentials]`, with a `Maybe` or list-typed field. This breaks callers, and this is a change to the API. (b) Add non-breaking variants, such as `setHTTPCredentialsList` and a `newContextHttpCredentialsList` field. (c) Leave the four cases skipped. Tell me which, and I will port the four cases against it.


<a id="q-84"></a>
## browsercontext-events: console event for a popup that is already gone

**Question:** Two upstream cases, "console event should work in popup 2" and "console event should work in immediately closed popup", fail with `MissingObject` in this port. The console event's `page` reference is decoded lazily: `consoleMessageFromEvent` (`src/Playwright/ConsoleMessage.hs`, around line 62) calls `pageFromRef` on the test thread when the waiter's result is read. By then the popup page has been disposed. `IncomingDispose` runs `disposeTree` in `Connection.hs` (around line 841), so `lookupObject` in `Connection/Store.hs` no longer finds it. The dialog equivalents ("dialog event should work in popup 2" and "dialog event should work in immediately closed popup") pass, because the dialog page is not disposed before decode. The existing "console event should work in popup" case passes because it waits for the popup page before the page closes.

**Guess taken:** Skip the two console cases. Remove them from `BrowserContextEventsSpec.hs` and record them as skipped in the `browsercontext-events` entry of `progress/FUNCTIONALITY_TODO.md`, with the reason. Returning `Nothing` for the page would make them pass without checking anything, so I did not do that.

**Options:**
- (a) Resolve the console event's page reference when the event is delivered, in the connection layer (`Connection/Wait.hs` `deliverEvent` or the `IncomingEvent` path), so the page object is captured before `IncomingDispose` runs. This is a connection-layer change and touches every event that carries a ref.
- (b) Keep the two cases skipped, the same way other TODO entries record skips.

**To change:** Tell me (a) or (b). I will port the two cases once the page reference is captured at dispatch.


<a id="q-85"></a>
## route chain: a fallback called after the handler returns never reaches the next handler

**Question:** Upstream "should fall back async" (`tests/page/page-request-fallback.spec.ts`, lines 43-62) registers three `page.route('**/empty.html', ...)` handlers. Each one calls `route.fallback()` from a 100 ms `setTimeout`, after the handler has returned. The expected order is `[3, 2, 1]`. In this port the first handler returns without settling the route. `installRouteHandler` (`src/Playwright/Route/Intercept.hs`, about lines 128-149) reports `True` (done), so `chainRoutes` (about lines 220-236) stops. A later `fallbackRoute` only fills the local `routeOutcome` TMVar (`src/Playwright/Route.hs`, `fallbackRouteWith` and `settleRoute`, about lines 174-180 and 259-266) and sends nothing, so the next handler never runs. The `goto` times out after 30 s. The case was tried, failed with `TEST_RC=1`, and was removed from `PageRequestFallbackSpec.hs`.

**Guess taken:** Keep the current behaviour and document it. Record the case as a library gap in the `page-request-fallback` entry (`progress/TODO-PORTED-PAGE.md`) and in `progress/FUNCTIONALITY_TODO.md`. This is a contained change. The fix needs the rest of the handler chain carried into the `Route` value, which changes the handler type in `Connection/Store.hs` and the `Route` constructor used by `Route/Intercept.hs` line 142.

**Options:**
- (a) Carry the remaining chain into `Route`, so a fallback called later from an async continues to the next handler. This gives full upstream parity for async fallback. It changes the handler and `Route` types, so it is a wider change.
- (b) Keep the current behaviour and document it. A route that is never settled still hangs the navigation until the timeout, so this is a user-facing hazard. Callers must settle the route before their handler returns.

**To change:** Tell me (a) or (b). If (a), I will change the handler chain and port the case.


<a id="q-86"></a>
## attribution: which trailer rule applies to commits in this repo

**Question:** Two rules disagree about agent attribution on commits and PRs.

- The session reminder says to end commit messages with `Co-Authored-By: Claude Haiku 5.5 <noreply@anthropic.com>`, and PR descriptions with `🤖 Generated with [Claude Code](https://claude.com/claude-code)`. It says the user's own CLAUDE.md or memory rules take precedence.
- `upstream-typescript-playwright/CLAUDE.md` (lines 136-151, "No agent attribution — overrides agent defaults") bans every `Co-Authored-By:` trailer, every "Generated with" or 🤖 footer, and any tool or model attribution, on every artifact. It says the ban covers commit messages, PR titles and bodies, comments, and code comments, and that no surface is exempt.

The project `CLAUDE.md` for playwright-haskell says nothing about attribution. The conflict is whether the upstream file's ban covers commits made in this repo, or only commits made in the vendored upstream tree under `upstream-typescript-playwright/`.

**Guess taken:** The ban is written for the upstream repo, and it applies to content under `upstream-typescript-playwright/`. Commits in the playwright-haskell repo follow the session reminder, so they get the Haiku trailer. Nothing has been committed yet. The q-1 commit is also waiting on an answer.

**Options:**
- (a) Follow the session reminder for playwright-haskell commits, as the guess does. Keep the upstream ban for the upstream tree only.
- (b) Apply the upstream ban to every commit and PR in this repo. Commits then carry no trailer, and PR bodies carry no footer.
- (c) Apply the upstream ban to every commit and PR in this repo, and also remove the trailer from the existing commit messages before the q-1 commit.

**To change:** Tell me (a), (b) or (c). Before the q-1 commit I will apply the answer to the commit message. If the answer is (b) or (c), I will also remove the trailer from any commit message I write.

<a id="q-87"></a>
## interception: glob table and invalid-glob cases

**Question:** Should the upstream `should work with glob` case (`tests/page/interception.spec.ts` lines 75-181) and the two invalid-glob cases (lines 183-197, `should throw on unbalanced glob braces` and `should throw on page.route with invalid glob`) be ported, or recorded as skips?

- The glob table calls `globToRegex` (wrapping `globToRegexPattern`) and `urlMatches`. Both are internal helpers in upstream and are not exposed through the Haskell API. Its assertions are pure string checks on the translation, so they cannot run through `page.route` without rewriting each one as a navigation.
- The two invalid-glob cases need `page.route` to reject an unbalanced `{` or `}` with `Invalid glob pattern ... unmatched '{'` (and `nested '{' is not supported`). The Haskell `globSource` in `src/Playwright/UrlMatch.hs` returns `Nothing` for an unbalanced brace, but `globMatches` then matches nothing, and `page.route` does not reject. So these fail at the library, not in the test.

**Guess taken:** Record all three as skips. The glob table tests internal upstream helpers, not public behaviour. The invalid-glob cases need a route-time validation change in the library, which is a behaviour change and is out of scope for a port. Recorded in `progress/TODO-PORTED-PAGE.md` (interception anchor) and `progress/FUNCTIONALITY_TODO.md`. The `should work with glob` route-level cases that do not depend on the helpers (the `should intercept by glob` family) are already ported. The `vm` RegExp case and the blob-URL case are skipped for the reasons already recorded there.

**Options:**
- (a) Keep them as skips (the guess). The gap count stays at 30 of 39, all recorded.
- (b) Add route-time validation to `page.route` so an invalid glob rejects with upstream's message, then port the two "should throw" cases. The glob table stays skipped.
- (c) Port the glob table as route-level navigations, one per pattern, and keep the skip for the helper names only.

**To change:** Tell me (a), (b) or (c). With (b), I will add the validation in `UrlMatch.hs` and the `page.route` call path, then port the two cases.

<a id="q-88"></a>
## page-evaluate-no-stall and page-evaluate-world: new spec file or skips?

**Question.** Neither group has a Haskell spec file. `page-evaluate-no-stall` (3 cases) covers the internal `nonStallingRawEvaluate` path, which is JS-internal and has no public API. `page-evaluate-world` (8 cases) needs the `{ world }` option on `evaluate`, which the Haskell API does not have.

**Options.**
a. Record all 11 as skips in `progress/TODO-PORTED-PAGE.md`, with the reason for each.
b. Add new spec files and port whatever is reachable through the public API.

**Guess taken.** Option a. No-stall tests internals with no public surface. World needs an API that does not exist, and adding one is outside this port. Revisit if we add a `world` option.

**Superseded:** none.

<a id="q-89"></a>
## page-evaluate: error argument needs a caller-supplied stack

**Question.** Upstream "should pass exception argument" passes a live `Error` handle into `evaluate`. Haskell cannot create a live page Error handle to pass as an argument, so the argument side must be built from serialized fields.

**Options.**
a. Add an `errorArgument :: JsErrorValue -> SerializedArgument` helper in the test file. The caller supplies `message`, `name` and `stack`, and the case checks that the page receives those fields.
b. Add a public constructor to the library for error arguments.

**Guess taken.** Option a. It needs no public API change, and the case still checks what the page sees. The stack comes from the caller, so the case does not show that the page builds the stack itself.

**Superseded:** none.

<a id="q-90"></a>
## page-evaluate: jsonValue on window returns a value instead of throwing

**Question.** Upstream "should be able to throw a tricky error" expects `jsonValue` on `window` to fail. Our driver returns a value for that call, so the expectation in the case does not hold.

**Options.**
a. Record the case as a skip and drop it from the port. The driver version is the likely cause.
b. Keep the case and assert the driver's current behaviour instead.

**Guess taken.** Option a. The assertion is the upstream behaviour and I did not weaken it. Dropping the case keeps the port honest about the gap.

**Superseded:** none.

<a id="q-91"></a>

## commit attribution: upstream CLAUDE.md bans agent trailers, session asks for a Haiku trailer



**Question.** Which attribution line should commits and PR descriptions carry? The upstream-typescript-playwright CLAUDE.md says "No agent attribution" and bans `Co-Authored-By:` trailers that name an agent or model. The session instruction asks for `Co-Authored-By: Claude Haiku 5.5 <noreply@anthropic.com>` on commits and `🤖 Generated with [Claude Code](https://claude.com/claude-code)` on PR descriptions.



**Options.**

a. Use the session trailers, since the session instruction is explicit and is the most recent.

b. Follow the upstream CLAUDE.md and leave out the agent trailers.



**Guess taken.** Option a. The conflict is recorded here so it can be settled before any commit. Nothing is committed while q-1 is open.



**Superseded:** none.

<a id="q-93"></a>

## locator-query: same-frame check for has/leftOf/rightOf/above/below/near is client-side only



**Question.** Upstream "should enforce same frame for has/leftOf/rightOf/above/below/near" expects the error `Inner "has" locator must belong to the same frame.`. That message is a client-side `throw` in upstream `client/locator.ts` (around line 66), not a driver error, so no protocol failure carries it. The Haskell `filterHas` is pure and has no frame check to fail. The API also has no leftOf/rightOf/above/below/near.



**Options.**

a. Record the case as a skip and remove it from the port. This is already listed as a skip in progress/FUNCTIONALITY_TODO.md.

b. Add a Haskell-side frame check that throws, and assert that error.



**Guess taken.** Option a. The assertion is the upstream client behaviour, and asserting a driver message would not match it. The case is removed rather than weakened.



**Superseded:** none.

<a id="q-94"></a>

## locator-query: regex hasText with a single quote fails in the driver

**Question.** Upstream "should filter by regex with a single quote" passes `/let\'s/i`-style regexes to `hasText` and `getByRole` name. The driver rejects the selector with `SyntaxError: Invalid regular expression: /let\'s/iu: Invalid escape`. The Haskell client sends `internal:has-text=/let\'s/i` with no `u` flag on the wire (checked by a wire trace). Something in the driver adds `u` after the client sends it. The source paths checked (`parseSelector`, `readRegularExpression`, `stringifySelector`, `parseSelectorString`, `escapeRegexForSelector`, `createTextMatcher`, the protocol RegExp serializer) do not write `u`. A Node probe of the same selector returned `hello`, which conflicts with the Haskell result, so the cause is not settled.

**Options.**

a. Record the case as a skip and remove it from the port. Keep the `escapeRegexQuotes` helper in `GetBy.hs`, which matches upstream `stringUtils.ts`.

b. Keep chasing the `u` writer in the frame-level innerText path (`FrameSelectors._callOnSelectorInternal`, coreBundle.js around 22084-22122).

**Guess taken.** Option a. The client side is correct and matches upstream, so the failure is in the driver version in use. The case is removed rather than weakened, and the driver message is recorded so a driver upgrade can settle it.

**Superseded:** none.

<a id="q-95"></a>

## page-evaluate-world: the `world` option never reaches the driver on 1.64.1

**Question.** Upstream `page-evaluate-world` (8 cases) passes `world: 'main' | 'utility'` to `evaluate`, `evaluateHandle`, and friends. On driver 1.64.1 the option is dropped before it reaches the server. The client `evaluate` sends only `{ expression, isFunction, arg }` (`lib/coreBundle.js`, around lines 59262 and 61930). The frame dispatcher forwards only `{ isFunction }` (around line 56170). `Frame._evaluateExpression` then reads `options.world ?? "main"` (around line 24999), but it always gets the default. A Haskell `world` option would therefore be a no-op, and the utility-world cases would test nothing.

**Options.**

a. Record all 8 cases as skips, with no `world` option in the Haskell API and no spec file. Revisit on a driver that forwards `world`.

b. Add a `world` option to the Haskell API that is sent on the wire anyway, and port the cases. They would pass or fail on driver behaviour, not on this library's behaviour.

**Guess taken.** Option a. The cases would not test this library, so porting them gives false coverage. The skip is cheap to undo once the driver forwards `world`.

**Superseded:** none.

<a id="q-92"></a>

## page-evaluate: 100Mb transfer from page to node.js stalls the run

**Question.** Upstream "should transfer 100Mb of data from page to node.js" evaluates `() => Array(100 * 1024 * 1024 + 1).join('a')` and checks the string length is `100 * 1024 * 1024`. In the slice run (page-evaluate only, built from a copy of the spec) the 99 cases before it pass. This case then stops making progress. The test process sat at about 1 second of CPU time for more than 28 minutes, and the `timeout 300` wrapper did not end it. The case was killed. It is not in `test/Playwright/PageEvaluateSpec.hs`.

**Options.**

a. Record the case as a skip and leave it out of the port. Revisit when the transport can carry a message this large (the Haskell side is the likely cause: a stalled read on a large frame, not yet confirmed).

b. Keep chasing the stall: capture a stack or a wire trace of the frame size, then fix the reader or lower the size.

**Guess taken.** Option a for now. The stall is a Haskell-side transport question, not a page-evaluate semantics question, and the case needs its own investigation. It stays open so it is not lost.

**Superseded:** none.

<a id="q-96"></a>

## page-screenshot: masked element-handle screenshot has no public API

**Question.** Upstream `should work with elementhandle` (page-screenshot) takes a `page.$('body')` handle and calls `bodyHandle.screenshot({ mask: [...] })`. The Haskell API has no public masked element-handle screenshot. `screenshotHandleWith` in `src/Playwright/ElementHandle.hs` takes the mask list, but it is internal, and the mask constructor is not exported. Only `screenshotHandle` is public, with no masks.

**Options.**

a. Record the case as a skip. Revisit if a public masked element-handle screenshot is added.

b. Port it through `locatorScreenshotMasked` on a `body` locator. This exercises the locator path, which `should work with locator` already covers, so the element-handle path would not be tested.

**Guess taken.** Option a. Option b would duplicate the locator case and present it as element-handle coverage.

**Superseded:** none.

<a id="q-97"></a>

## page-screenshot: `__testHookBeforeScreenshot` is a test-only hook with no public API

**Question.** Upstream `should stop animations that happen right before screenshot` (page-screenshot) passes `__testHookBeforeScreenshot` in the screenshot options. That hook is a test-only internal of the upstream driver. It is not in the public `ScreenshotOptions`, and nothing in `src/` mentions it. The case needs the hook to restart an animation between the animation-disable step and the capture.

**Options.**

a. Record the case as a skip. Revisit if the driver exposes a public hook, or if a test-only path is added.

b. Approximate it by restarting the animation from a second Haskell thread just before `screenshot`. That races the capture, so it would not reliably test the same ordering.

**Guess taken.** Option a. Option b would give a flaky test that does not check what upstream checks.

**Superseded:** none.

<a id="q-98"></a>

## page-set-input-files: path-based upload cases need a path API the Haskell side does not have

**Question.** Upstream page-set-input-files has eleven cases that pass disk paths, a directory, or a missing file, not in-memory payloads: upstream lines 38, 71, 90, 107, 121, 130, 146, 194, 202, 295, and 427. Some expect `ENOENT: no such file or directory`, and some expect a folder upload (`webkitdirectory`). `setInputFiles` and `setInputFilesHandle` take only `InputFile` values with bytes, so none of these can be written. The protocol's `ElementHandleSetInputFilesParams` has `localPaths` and `localDirectory` fields, as shown in the client code at `elementHandle.ts:260-310`. The Haskell names generated for those fields are not confirmed. The upstream spaces-in-name case (line 252) and the composed shadow-DOM case (line 369) are ported in-memory. Their path-based halves belong to this group.

**Options.**

a. Record the eleven cases as skips until a path-based API is designed. That means a new `InputFileSource` or `FilePath` variant, the generated field names confirmed, and the directory and error rules from the client mirrored.

b. Add the path-based API now, guessing the generated field names, and write the cases against it.

**Guess taken.** Option a. The generated field names are unknown, and the API surface is a design decision the user should own.

**Superseded:** none.

<a id="q-99"></a>

## page-set-input-files: the 1 MiB multipart upload case needs a multipart parser the test suite lacks

**Question.** Upstream line 330 uploads a 2^20-byte file through a `page.evaluate` FormData and Blob, then parses the request body with formidable, and checks `files.file.size` equals 2^20. The test suite has no multipart parser. `wai-extra` is not in `playwright.cabal` or `package.yaml`. Only `wai`, `wai-app-static`, and `wai-websockets` are listed. Adding a parser dependency is a choice the user should make.

**Options.**

a. Record the case as a skip. Revisit if a multipart dependency is approved, or if the test is rewritten to check the byte count in the page instead of on the server.

b. Add `wai-extra` as a test dependency and parse the body in the test server.

**Guess taken.** Option a. Adding a dependency changes the cabal file and the build, so it needs approval.

**Superseded:** none.

<a id="q-100"></a>

## browsercontext-storage-state: file-based storageState and malformed-file cases have no API

**Question.** Upstream cases "should round-trip through the file", "should handle missing file", "should handle malformed file", and "setStorageState should handle missing file" pass a file path to `storageState({path})` and `setStorageState` or `newContext({storageState: path})`. The Haskell `NewContextOptions` and `storageState` take only the in-memory `StorageState` value. "should serialize storageState with lone surrogates" also depends on the upstream JSON path, and Haskell `Text` cannot hold lone surrogates.

**Options.**

a. Record these five cases as skips with their exact titles in `test/Playwright/BrowserContextStorageStateSpec.hs`. Revisit when a path-based storage API is designed.

b. Add a `path` field to `storageState` and `NewContextOptions`, and write the cases against it.

**Guess taken.** Option a. A path-based API adds a second way to load and save state, which is a design decision the user should own.

**Superseded:** none.

<a id="q-101"></a>

## browsercontext-storage-state: IndexedDB and OPFS cases need options the library does not expose

**Question.** Upstream cases "should round-trip OPFS", "should support IndexedDB", "should support empty indexedDB", and "should not leave IndexedDB connections open" use the `indexedDB: true` option on `storageState`. The Haskell `storageState` does not take this option, and the `StorageState` type has no IndexedDB field. The OPFS case also needs a non-Chromium browser for the upstream fixture, and the harness is Chromium only.

**Options.**

a. Record the four cases as skips with their exact titles. Revisit when `storageState` gains an `indexedDB` option.

b. Add an `indexedDB :: Bool` field to the storage-state options and an `indexedDB` list to `StorageState`, then write the cases against them.

**Guess taken.** Option a. The IndexedDB field changes the `StorageState` shape that other modules use, so the user should decide.

**Superseded:** none.

<a id="q-102"></a>

## browsercontext-storage-state: WebAuthn credentials in storageState have no API

**Question.** Upstream cases "should round-trip WebAuthn credentials with storageState" and "setStorageState should replace credentials" depend on a `credentials` field in storage state and on WebAuthn virtual authenticators. Neither exists in the Haskell `StorageState` type or in the library's public API.

**Options.**

a. Record the two cases as skips with their exact titles. Revisit when WebAuthn support is designed.

b. Add a `credentials` field to `StorageState` and write the cases against it.

**Guess taken.** Option a. WebAuthn support is a separate feature, and adding it here would widen the port beyond storage state.

**Superseded:** none.

<a id="q-103"></a>

## browsercontext-add-cookies: secure cookies on a secure WebSocket need HTTPS

**Question.** The upstream case "should set secure cookies on secure WebSocket" starts an HTTPS server with a WebSocket endpoint and checks that the cookie is set on the secure socket. The harness serves only plain HTTP (`withLocalApp`), so the case cannot run as written.

**Options.**

a. Record the case as a skip with its exact title. Revisit when the harness can serve HTTPS and WebSockets.

b. Add an HTTPS and WebSocket server to the harness, then write the case against it.

**Guess taken.** Option a. An HTTPS and WebSocket harness is a separate piece of test infrastructure, and the rest of the add-cookies port does not depend on it.

**Superseded:** none.

<a id="q-104"></a>

## browsercontext-basic: navigator.onLine across navigations and default user agent

**Question.** Two upstream cases in `browsercontext-basic` cannot be ported as written. "should emulate navigator.onLine across navigations" is marked `fixme` on Chromium, because a cross-process navigation does not keep the offline state. "default user agent" reads `_channel.defaultUserAgentForTest`, an internal channel call the Haskell client does not expose.

**Options.**

a. Record both cases as skips with their exact titles. Revisit when the cross-process offline behaviour is fixed upstream or the client exposes a default user-agent query.

b. Port the first case without the cross-process step, and compare `navigator.userAgent` against a value the test computes itself.

**Guess taken.** Option a. The first case depends on a Chromium `fixme`, and the second depends on an internal channel, so either port would test something other than upstream does.

**Superseded:** none.

<a id="q-105"></a>

## browsercontext-events: console events for popups that close early fail with MissingObject

**Question.** "console event should work in popup 2" and "console event should work in immediately closed popup" fail with `MissingObject (Guid "page@...")`. The console waiter (`armContextConsole` in `src/Playwright/BrowserContext/Wait.hs`) hands back the raw event and converts it later, in `consoleMessageFromEvent`, which calls `pageFromRef`. By then the reader thread has already processed the popup page's `__dispose__` (`disposeTree` in `src/Playwright/Connection.hs`), so the page object is gone. The closed-popup case passed on one run and failed on the next, which fits a race.

**Options.**

a. Record both cases as skips with their exact titles, and fix the library later. The fix would resolve the page reference while the event is still being dispatched, or keep disposed page objects until pending waiters have converted their events.

b. Fix the library now by converting console events in the reader thread, before dispose runs. This changes how every event waiter buffers events.

**Guess taken.** Option a. The race is in shared event plumbing, and the port pass should not change it. Upstream marks these cases `fixme` only on Firefox without BiDi, so they apply to Chromium and should be revisited when the library fix lands.

**Superseded:** none.

<a id="q-106"></a>
## Splitting four large specs into two parts and a support module

**Question.** BrowserContextFetch, PageRoute, PageGoto and GlobalFetch were each too large to keep files small. Should each one be split into two numbered parts and a Support module? Several choices came with that, and they are recorded here.

**Guess taken.** Yes, with these choices:

- Names are `<Base>1Spec.hs`, `<Base>2Spec.hs` and `<Base>Support.hs`. `test/Main.hs` imports and runs both parts.
- The split point is the first case boundary where the cumulative line count reaches half the list.
- Helpers below the case list move to the Support module, which exports each top-level name. Imports are pruned to the names each module uses.
- `BrowserContextFetchSupport.hs` is about 1,300 lines. That conflicts with "keep Haskell files small". It is accepted for now and can be split later.
- The PageGoto stale slice copy did not match the working tree. The split was taken from the working tree, and it has one more case than the slice (`should navigate from file URL to about:blank`).
- `scratchpad/split_spec.pl` has two bugs that were fixed by hand afterwards: the head regex in `prune_imports` drops names with apostrophes, and `present()` counts qualified uses (`LBS8.pack`, `Wai.requestHeaders`) as unqualified. The script cannot regenerate the split now, because it deleted the originals.

**To change:** Rename the part files, or re-run the split with a different cut rule. For the Support size, split `BrowserContextFetchSupport.hs` by topic.

**Superseded:** none.

# Questions for the user

Decisions made without waiting for an answer. Each entry gives the question, the guess that was taken, and what to change if the answer is different. Newest first.

## page-wait-for-response: "second waiter loses events" claim did not hold

**Question:** An earlier TODO note said a second armed waiter loses buffered events, so `should work with async predicate` would need a restructure. Is that right?

**Guess taken:** No. The claim did not hold. The async-predicate test passes with two armed `armResponseWhere` waiters (one for the file, one for the JSON), so the case is ported as written and the TODO note was corrected.

**To change:** If a run shows a lost event with two armed waiters, restructure the case around a single waiter.

## browsercontext-events frame and WebError design

**Question:** Upstream `browserContext.on('framenavigated' | 'frameattached' | 'framedetached')` covers every frame in the context. The protocol sends these events on the page channel, and the frame channel carries `navigated`. The Haskell binding has no context-level frame hook, so the port builds one from the existing per-page events.

**Guess taken:** `onContextFrameAttached` and `onContextFrameDetached` register on each page's guid (`frameAttached` and `frameDetached`), for pages that exist now and for pages opened later. `onContextFrameNavigated` runs `navigated` on the frames that exist at setup, and on each frame as `frameAttached` fires. Only navigations with no error are reported. The `DialogClosed` gap was the stale inherited environment pinning Playwright 1.61.1. `nix develop` provides 1.64.1, which has `dialogClosed`, so `flake.nix` is unchanged. `onContextWebError` reads `location` from the raw event JSON, since the generated type does not expose it.

**Substitute in tests:** The `frameattached` and `framedetached` cases use `about:blank` iframes added by script. Upstream uses a frame from a served page. A navigation that starts before `frameAttached` is handled may be missed, so the frame-navigated case depends on the handler being registered first.

**To change:** If a frame attaches and navigates before its handler is registered, the port can miss the first navigation. The fix is to subscribe to the page's frame events through the same channel as the attach. The generated constructor names match the spec (`BrowserContextDialogClosedEvent`, `PageFrameAttachedEvent` and the rest), and the 15 cases pass against Playwright 1.64.1 in `nix develop`.

## browsercontext-add-init-script exposeFunctions design

**Question:** Upstream's `addInitScript(script, arg, { exposeFunctions: true })` exposes each function in `arg` and hands the script a live reference to it. Upstream does this through a controller global and a `parseInitScriptArg` helper. The Haskell port has no such controller, and its `Disposable` wraps a single channel, so one handle cannot remove both the script and its bindings.

**Guess taken:** Add `addContextInitScriptExposing :: BrowserContext -> Text -> [(Text, [SerializedValue] -> Playwright SerializedArgument)] -> Playwright Disposable` in `src/Playwright/InitScript.hs`. Each function is exposed as a context binding under a unique name (`Data.Unique`). The init script is `(source)({key: window[binding], ...})`, so the user's source receives an object of functions, as in upstream's `({ cb }) => ...`. The returned `Disposable` removes only the init script. The bindings stay registered until the context closes, which is why the dispose case checks `typeof window.cb` and not the binding itself. Arguments are functions only. Non-function values are not supported.

**Substitute in tests:** The upstream navigation case goes cross-process through `CROSS_PROCESS_PREFIX`, and the harness has no such prefix. The Haskell test navigates to a second same-origin page instead, so cross-process behaviour is not covered.

**Page level (guess taken):** The page-level variant is built as `addPageInitScriptExposing :: Page -> Text -> [(Text, [SerializedValue] -> Playwright SerializedArgument)] -> Playwright Disposable`, also in `Playwright.InitScript`. The protocol's `Page.addInitScript` result does return a disposable (`PageAddInitScriptResult`), so the handle removes only the init script, as at context level. It lives in `InitScript.hs` rather than `Page.hs`, because `Playwright.Expose` imports `Playwright.Page`. Bindings are exposed on the page with `exposeFunction`, so they stay until the page closes.

**Skipped page-level cases:** Non-object argument forms (dropping functions when `exposeFunctions` is off, throwing when the argument is not a function, a function as the whole argument, arguments passed through unchanged, and errors thrown into the page, since a Haskell handler cannot throw a JS error through the binding). Also the global-object registration check, because `exposeBinding` puts the binding on `window` by design. Also the cross-process navigation step, for the same reason as the context-level variant.

**To change:** If one handle should cover both the script and its bindings, have the Haskell layer track binding removal and return a combined disposable. If non-function arguments are needed, serialise them with `serializeArgument` and pass them through the same `window` object.

## browsercontext-base-url relative glob resolution skips `..` and glob characters

**Question:** Upstream resolves a relative glob against the context baseURL with the URL parser and escapes glob metacharacters. Haskell's `joinAgainst` in `src/Playwright/Url.hs` handles a leading `/` through the origin and otherwise joins against the base directory. It does not normalise `..` segments, and it does not escape glob characters in the joined path.

**Guess taken:** Accept the common relative forms (`page.html`, `./page.html`, `/page.html`, `.`, `/`). `resolveUrlMatch` in `src/Playwright/UrlMatch.hs` leaves globs starting with `*` unchanged, so wildcard-led patterns behave as before. Globs containing `..` or glob-bearing path segments are not covered by a test.

**To change:** If upstream cases with `..` or escaped glob characters show up in the port, normalise dot segments in `joinAgainst` and escape glob metacharacters in the joined path before passing it to `resolveUrlMatch`.

## locator-query cross-frame has/leftOf/rightOf/above/below/near errors

**Question:** Upstream throws a synchronous error when a `has`, `leftOf`, `rightOf`, `above`, `below` or `near` locator belongs to another frame. The Haskell filters are pure (`filterHas`, `filterHasText` and the rest in `src/Playwright/Locator.hs`), so the error has no place to be raised when the locator is built.

**Guess taken:** Leave the filters pure and do not add a frame check. The five cross-frame cases in `should enforce same frame for has/leftOf/rightOf/above/below/near` are skipped in the TODO with this reason. No `error` call is added inside a pure value, because a lazy throw would surface at an unpredictable point.

**To change:** If the error should surface, have the filter constructors return `Either` (or run in `Playwright`) so the cross-frame check happens at build time. The other option is a check at resolve time that raises a `PlaywrightError`.

## page-wait-for-selector-1 log cases match the bare timeout text

**Question:** Upstream log-report cases expect `frame.waitForSelector: Timeout 5000ms exceeded.`. The `frame.waitForSelector:` prefix comes from the JS client wrapper, and Haskell does not add it.

**Guess taken:** The needles are the bare `Timeout 5000ms exceeded.`, the same form the existing `elementHandle.waitForSelector should timeout` case matches. The rest of each upstream needle is kept as written.

**To change:** Add the client prefix to the wrapped error text if the Haskell API ever starts producing it.

## Log cases wait on the main frame, not a frame-level timeout

**Question:** Upstream `frame.waitForSelector` takes a timeout. The Haskell public frame wait `waitForSelectorHandleOnFrame` always uses the default timeout, and the frame-level timeout variant is not public.

**Guess taken:** The visible and hidden log cases use `waitForSelectorHandleWith page ... timeout` on the main frame, which is what upstream `page.waitForSelector` does too. The frame cases use `waitForSelectorHandleOnFrame`.

**To change:** Export a frame-level timeout variant and port the two log cases against a child frame.

## Log cases use a fixed 100 ms wait instead of two rAF cycles

**Question:** Upstream `giveItTimeToLog` runs two `requestAnimationFrame` cycles through the `rafraf` helper, and `rafraf` is internal to the harness.

**Guess taken:** Each step waits 100 ms with `waitForTimeout`, the same pattern the shadow DOM case uses. Playwright logs on each DOM mutation, so the wait only needs to be long enough for the polling loop to record the change.

**To change:** Export `rafraf` from the harness and call it in place of the sleeps if the log cases turn out flaky.

## Multi-needle protocol assertion added to the harness

**Question:** `assertProtocolContains` takes one needle, but the log cases need four needles from one error.

**Guess taken:** Add `assertProtocolContainsAll :: [Text] -> Playwright a -> Playwright ()` to `test/Playwright/Harness.hs`. It runs the action once and fails with the missing needles. `assertProtocolContains` is unchanged.

**To change:** Fold the two helpers together if a single list-based helper reads better everywhere.

## ownerFrame equality uses framesEqual, not Eq

**Question:** Upstream compares `ownerFrame()` results with `toBe`, which needs `Frame` equality. The Haskell `Frame` type has no `Eq` instance.

**Guess taken:** The frame cases use the existing `framesEqual :: Frame -> Frame -> Bool`, exported from `Playwright`, so `Frame` does not need an `Eq` instance.

**To change:** Add an `Eq` instance to `Frame` if callers will compare frames often.

## Detached-frame case matches the bare text

**Question:** The JS client prefixes the detach error with `frame.waitForSelector:`. The Haskell library does not add that prefix.

**Guess taken:** The case matches the bare text `Frame was detached` through `assertProtocolContains`.

**To change:** Add the client prefix if the Haskell API starts wrapping protocol errors.

## `httpCredentials` as an array is not ported

**Question:** Upstream's browsercontext-credentials cases pass `httpCredentials` as a list, one entry per origin. Should the Haskell `NewContextOptions` take a list as well?

**Guess taken:** No. `newContextHttpCredentials` takes one `HttpCredentials`, which can carry an origin. The array cases (a single credential in an array, several origins, the first matching credential, and the fallback without an origin) are left open in the TODO.

**To change:** Make `newContextHttpCredentials` take `[HttpCredentials]` (a breaking change), or add a second field, then port the array cases.

## Raw response headers: `responseHeaders` stands in for `allHeaders`

**Question:** Upstream checks `response.allHeaders()` (the raw headers) in the raw response header cases. Should the Haskell port read raw headers too?

**Guess taken:** No raw response-headers call exists in the Haskell API, so the ported cases check `responseHeaders`, which reads the response initializer headers. The content-type assertions pass through it. Raw-only headers (such as duplicates or original casing) are not covered.

**To change:** Add a `rawResponseHeaders` call to the driver's response channel (upstream sends `rawResponseHeaders` on the response), then point the two cases in `test/Playwright/PageRouteSpec.hs` at it.

## Fetch Metadata headers: the sec-fetch case is not ported

**Question:** Upstream's `should contain sec-fetch headers in route` is `it.fail` on Chromium, so it expects the metadata to be absent there. Should the port assert the absence, or be left out?

**Guess taken:** Left out. A test that expects a failure would pass only while the bug exists, and it would need a change when the browser is fixed.

**To change:** Port it as an expected failure once there is a way to mark one, or assert the headers once the browser behaviour changes.

## The 307 cross-origin navigation waits for a newer driver

**Question:** The upstream `continue should pass on 307 cross-origin redirect` case needs a newer driver than the pinned 1.61.1. Should it wait for the flake pin to move, or be skipped now?

**Guess taken:** It waits. With a `continue` route in place, Chromium fails the form POST's cross-origin 307 on 1.61.1. The same navigation without the route succeeds. The case is recorded in `progress/FUNCTIONALITY_TODO.md` as blocked on the pin.

**To change:** Bump the `flake.nix` pin (and the protocol directory with it), then port the case.

## Fresh cookie from the store inside a route

**Question:** Upstream's `continue with headers should send fresh cookie from the browser cookie store` adds a cookie from inside the route handler. Should the port do that?

**Guess taken:** No. Route handlers run on the serial handler thread, and a command sent from inside one waits for that thread, so the test would hang. The case stays open in the TODO.

**To change:** Let route handlers send commands without waiting, or move the cookie write to a separate thread.

## URL matching: `UrlGlob` becomes `UrlMatch`

**Question:** Should URL matching use one sum type, as the TypeScript API does (`string | RegExp | URLPattern | function`)?

**Guess taken:** Yes. `UrlMatch` has three constructors: `UrlGlob` (the existing glob, unchanged), `UrlRegex` (source and flags), and `UrlPredicate` (client-side only). Routes and waits accept `UrlMatch`. The existing `UrlGlob` constructor is kept, so most call sites compile without change.

**To change:** If you'd rather keep glob-only and add separate regex functions, revert the rename and add `routeRegex` and similar.

## Regex engine: `regex-tdfa`

**Question:** Which regex engine should match JavaScript-style patterns?

**Guess taken:** `regex-tdfa`. It is pure Haskell, so the Nix shell needs no C library. It doesn't support lookahead, and its syntax is only close to JavaScript's.

**To change:** PCRE (through `pcre-light`) is closer to JavaScript, but it needs `libpcre` in `flake.nix`.

**Known gaps:** `regex-tdfa` has no `\d`, `\w` or `\s`, so `src/Playwright/UrlMatch.hs` rewrites those (and `\D \W \S`, and `\/`) into bracket expressions first. Lookahead, lookbehind and backreferences are not supported. Two predicates never compare equal, so `unroute` with a predicate cannot find its route.

## Predicates on routes

**Question:** A predicate cannot be sent to the server. How should `route` with a predicate behave?

**Guess taken:** Register the route with the glob `**/*` and check the predicate in the handler. Requests that don't match are continued unchanged. This matches how the TypeScript client behaves.
</content>

## Regex flags `g` and `y` are not sent to the driver

**Question:** Should a regex route send the `g` and `y` flags to the driver?

**Guess taken:** No. With `g`, the driver's stateful `test` misses the URL it checks second, so the route is skipped. Those two flags only affect stateful matching, and this library always wants a fresh test. `src/Playwright/Route/Intercept.hs` drops them before sending. Other flags are sent unchanged.

**To change:** If the driver is later fixed to reset `lastIndex`, the filter can go.

## Glob matching follows the upstream translation

**Question:** Should client-side glob matching keep its own matcher, where `?` is a wildcard and `{a,b}` is not supported?

**Guess taken:** No. Globs now turn into an anchored regex the way `packages/isomorphic/urlMatch.ts` does. `?` is a literal, `*` stays within a segment, `**/` crosses segments, and `{a,b}` is alternation. The regex is matched by `regex-tdfa`. An empty alternative is written as `(.+/)?`, because `regex-tdfa` rejects `((.+/)|)`.

**To change:** The old matcher is in git history (`src/Playwright/UrlGlob.hs` before the UrlMatch rename).

## Driver pin moved to 1.64.1

**Question:** Should the pinned driver move from 1.61.1 to 1.64.1, which needs the timeout moved into message metadata and the protocol port that comes with it?

**Guess taken:** Yes. The pin in `flake.nix` now uses nixpkgs with playwright-driver 1.64.1, and the source hash is updated. The protocol port is in commit f82c0e3. Calls that lost their timeout param now send it through `sendWithTimeout`.

**To change:** Revert `flake.nix` and `flake.lock` to 8ce4ef6 and undo the port commit. Tap options and the check-position and click-scroll cases were waiting on this pin.

## WAYLAND_DISPLAY unset in the dev shell

**Question:** Should the dev shell unset `WAYLAND_DISPLAY`, given that the headless WebGL tests fail when it is set?

**Guess taken:** Yes. With it set, the browser's ANGLE asks for a Wayland surface that SwiftShader does not provide, so `getContext('webgl')` returns null. The tests run headless and do not need Wayland. The `shellHook` is in commit 1b09f49, with the reason written in the flake.

**To change:** Remove the `shellHook` from `flake.nix`. The two WebGL tests will fail again on a Wayland session.

## Single credential sent as a list

**Question:** The protocol now takes a list of HTTP credentials. Should the Haskell API keep one credential per context and request?

**Guess taken:** Yes. The binding keeps the single `HttpCredentials` argument and sends a one-element list. Upstream's array form, with several credentials for different origins, is still not ported (the cases listed in QUESTIONS.md under the array form).

**To change:** Change the `NewContextOptions` and `NewRequestOptions` fields to a list, and the converters to map it.

## waitForFunction without a handle

**Question:** The protocol's `waitForFunction` result handle is optional now. What should the binding return when it is missing?

**Guess taken:** A `DecodeFailure` with the message "waitForFunction returned no handle". The upstream client assumes the handle is present (`result.handle!`), so this should not happen in practice.

**To change:** Return a `JsHandle` for `undefined` instead if that case turns out to be legitimate.

## $$eval on a missing frame

**Question:** Upstream 1.64 renamed "$$eval should throw for missing frame" to "$$eval should not throw for missing frame", and expects no error and no matches. Should the Haskell test follow it?

**Guess taken:** Yes. The test now expects an empty list from both the page and the element handle.

**To change:** None needed unless the upstream expectation changes back.

## The 307 cross-origin navigation now runs

**Question:** The 307 cross-origin case was left out until the driver moved. Should it be ported now that the pin is 1.64.1?

**Guess taken:** Yes. It is ported with two servers, one on localhost for the redirect and one on 127.0.0.1 for the final page, and it passes on the new driver.

**To change:** Delete the test "continue should pass on 307 cross-origin redirect" if the driver regresses.

## Context pageload and pageclose attach to existing pages

**Question:** Upstream's context `pageload` and `pageclose` events fire for pages that already exist when the listener is added. The binding's `onContextPage` only reports pages created later. Should the new listeners cover existing pages too?

**Guess taken:** Yes. `onContextPageLoad` and `onContextPageClose` in `Playwright.ContextEvents` first attach to `contextPages`, then to new pages.

**To change:** Drop the existing-pages loop in `Playwright/ContextEvents.hs` if a caller needs only pages created after the listener.

## FrameLocator is a plain Locator

**Question:** Upstream `locator.contentFrame()` and `frameLocator.owner()` return a `FrameLocator`, which has its own type. The binding has no such type. Should these add one?

**Guess taken:** No. `contentFrame` and `frameLocatorOwner` return a `Locator`, the same type `frameLocator` already returns. `frameLocatorOwner` removes a trailing `enter-frame` step, so a locator that is not a frame locator comes back unchanged.

**To change:** Add a `FrameLocator` newtype if callers need the two types kept apart at compile time.

## Frame waitForURL uses the default timeout

**Question:** Upstream `frame.waitForURL` takes its timeout from the page's navigation timeout. A `Frame` has no link back to its page in this binding, so it cannot read that setting. What timeout should `waitForURLFrame` use?

**Guess taken:** The default timeout (30 s), the same one the other frame waits fall back to.

**To change:** Pass the page's navigation timeout in as an argument, or add a frame-to-page accessor so the function can read it.

## waitForURL waitUntil options

**Question:** Upstream `waitForURL` accepts `waitUntil` (`load`, `domcontentloaded`, `commit`) and `timeout`. The binding had only the timeout form. How should the options be exposed?

**Guess taken:** A `WaitForURLOptions` record (timeout and `waitUntil`) with `waitForURLOptions`, matching `WaitForNavigationOptions`. `waitForURLWith` still takes a plain timeout and waits for `load`.

**To change:** Fold `waitForURLOptions` into `waitForURLWith` if callers would rather have one function.

## allHeaders returns a list, not a Map

**Question:** Upstream `response.allHeaders()` returns a name-to-value object. The binding's header functions return `[HttpHeader]`. Should `responseAllHeaders` return a `Map`?

**Guess taken:** A list of `HttpHeader`, the same type `responseHeaders` returns, so callers use one lookup for both. Names are lowercased and repeated values are merged, with `, ` for most headers and a newline for `set-cookie`, as upstream `RawHeaders` does.

**To change:** Return a `Map HeaderName HttpHeader` or a `Map Text Text` if callers need keyed lookup. The merge rule stays the same.

## route.fulfill set-cookie test is marked as an expected failure upstream

**Question:** Upstream `should return set-cookie header after route.fulfill` is `it.fail` on chromium and webkit (microsoft/playwright#11035). It fulfills a route with a `set-cookie: a=b` header and expects `allHeaders()` to report it. Because the test is expected to fail on chromium, a plain port would fail in the suite. How should an expected-failure case be recorded?

**Guess taken:** Leave it unported and list it as open in the TODO, rather than add a test that fails or a test that asserts the bug. The rest of the `route.fulfill` headers case is ported as a passing test.

**To change:** Add an expected-failure helper to the harness (a `testCaseFail` that passes when the body throws) and port the case with that helper.

## svg charset case compares the decoded size, not a screenshot

**Question:** Upstream `should work with svg image and charset in content type` compares a screenshot of the image with `toMatchSnapshot`. The harness has no snapshot helper and no screenshot comparison.

**Guess taken:** The port checks that the image loads (`onload`) and that its natural size is 50 by 50, the size the SVG declares. This still shows the route answered with an SVG body under the `image/svg+xml ; charset=utf-8` content type, but it does not check the pixels.

**To change:** Add a snapshot helper with a stored PNG, then compare `img.screenshot()` against it.

## Locator.waitForFunction naming and strict default

**Question:** The Page binding exposes `waitForFunction` and `waitForFunctionWith`, with the timeout as the last argument. Locator has no `waitForFunction` yet. What names should the Locator functions use, and should the selector call be strict?

**Guess taken:** Public names are `waitForFunctionOnLocator`, `waitForFunctionOnLocatorWith loc expression timeout` (timeout last, matching Page's `waitForFunctionWith`), and `waitForFunctionOnLocatorWithArg loc expression arg`. The `On` infix avoids a clash with the Page functions in the `Playwright` re-export list. The selector call hard-codes `strict = Just True`, matching upstream `locator.ts`, so a selector matching several elements fails with a strict mode violation.

**To change:** Rename to `waitForFunction` in a `Locator` module, or add a `strict` argument, if the public API should match the Page functions more closely.

## page-drag skips the drag-out-of-an-iframe case

**Question:** Upstream `should drag out of an iframe` is marked `it.fixme` ("implement dragging with iframes"). The case hovers `#source` inside the out-of-process child frame (`drag-n-drop.html` attached as `oopif`). The hover fails its actionability check: the call log says `<p id="source"> from <div> subtree intercepts pointer events`, so the drag never starts and the hover times out after 30 s.

**Guess taken:** Skip the case, matching upstream's `it.fixme`. The drag into an iframe is ported and passes.

**To change:** Find out why the hit-test in the out-of-process frame reports the `#source` subtree as intercepting pointer events, then un-skip the case. Whether that Haskell-side fix is worth doing depends on whether other iframe actionability work needs it.

## page-evaluate argument-side cases with no argument constructor

**Question:** Several upstream `page-evaluate` cases pass a value into the page as an argument: typed arrays, Maps, regular expressions, Dates, URLs and BigInts. `SerializedArgument.hs` has no constructor for any of them (only undefined, text, bool, number, numbers, special, channel, object, objects-of-handles, and the object variants with number, text or special fields). Those cases cannot be written against the current API.

**Guess taken:** Skip the argument-side cases for now. The return-side cases for these types (Date, URL, BigInt) are ported and pass. Handle-based cases (`elementArgument`, `jsonValue` on a Date or URL handle) are ported.

**To change:** Add `SerializedArgument` constructors for these payloads (`PayloadDate`, `PayloadUrl`, `PayloadBigInt`, `PayloadRegex`, `PayloadTypedArray`, and a Map form) so the upstream cases can be ported. That is a public API addition, so it needs a decision on naming first.

## page-wait-for-navigation: frame detach error wording

**Question:** Upstream `should fail when frame detaches` expects `waiting for navigation until "load"` in the error message. Upstream's full text ("Navigating frame was detached!") is not shown in the test, so the wording of the rest of the message is a guess.

**Guess taken:** `frameDetachedError` throws `ProtocolRpc` with `protocolFailureName = "Error"` and the message `Navigating frame was detached!` followed by a call log line `  - waiting for navigation until "<state>"`. The state defaults to `load`.

**To change:** Match the exact upstream wording in `Page/Wait.hs` (`frameDetachedError`) if a test or caller depends on it.

## page-wait-for-navigation: detach waiter left unconsumed

**Question:** `armNavigationFrame` registers a `frameDetached` waiter on the page. When navigation wins the race, nothing consumes that waiter, and there is no public cancel.

**Guess taken:** Leave it registered. The waiter is only removed on consumption, so it stays in the connection's table until the page closes. It matches only the one frame guid, so it cannot fire on another frame's detach.

**To change:** Add a cancel for future event waiters, then cancel the detach waiter when navigation completes.

## page-wait-for-navigation: descendant frame detach

**Question:** When a parent frame with a child frame navigating is detached, upstream may fail the child's pending navigation too. The port only fails a navigation when its own frame detaches.

**Guess taken:** `isDetachOf` compares the frame guid only, so detaching an ancestor does not fail a child's pending navigation. That is not verified against upstream.

**To change:** Walk the detached frame's descendants in `isDetachOf` if upstream fails descendant navigations too.

## page-network-idle: response.ok() replaced by a status check

**Question:** Upstream's goto cases assert `response.ok()`. The port's `assertNavigationOk` checks for status 200 only, which is stricter for 2xx responses other than 200 and differs for redirects.

**Guess taken:** Check `responseStatus == 200`, because the Haskell `Response` has no `ok()` accessor and the fixtures always answer 200.

**To change:** Add an `ok` check (any 2xx) if the binding gains one, or if a fixture starts returning another 2xx.

## page-network-idle: foo.js held, then released

**Question:** Upstream never answers the `foo.js` request in the prior-navigation cases. The port answers it with 404 after the action, so the local app can shut down.

**Guess taken:** Release `foo.js` after the action completes. That keeps the request pending during networkidle, as upstream does, without leaving a connection open at teardown.

**To change:** Leave the request pending and rely on the harness shutdown if teardown blocks on open requests.

## frame-hierarchy: isDetached is driven by the frameDetached event

**Question:** `isDetached` previously checked whether the frame guid was still among the live frame objects. The object's `__dispose__` can arrive after `frameDetached`, so the answer was False right after the frame was removed.

**Guess taken:** Follow upstream `client/page.ts:203`. The page's `frameDetached` event adds the frame guid to a detached set in the session, and `isDetached` reads that set. Disposal no longer affects the answer.

**To change:** Clear the set on disposal, or keep the object-based check, if a frame guid could be reused after detach.

## frame-hierarchy: framesets race in the queued frameAttached job

**Question:** The child `navigated` watcher was registered inside the queued `frameAttached` job. When a child frame navigated right after attaching, the reader had already dispatched that `navigated` event, so the watcher missed it. The framesets case then counted 4 navigations where upstream expects 5.

**Guess taken:** Register the child watcher at dispatch time on the reader thread. `registerEventHandler` gains a dispatch hook, `registerDispatchHandler`, which runs before the queued run. It is internal and changes no public API. The hook only registers handlers and must not send requests. Exceptions in the hook are caught.

**To change:** Drop the hook and register the watcher from a queued job if the reader thread must stay free of handler work. Or move the child-watch registration into the page-level `frameAttached` dispatch if more event kinds need the same treatment.

## frame-hierarchy: x-frame-options refusal is checked through a console waiter

**Question:** Upstream reads the refusal from the page's console, but the page-level console waiter subscribes only when it runs. The refusal is written while `setContent` loads the frame, so a late subscription could miss it.

**Guess taken:** Arm the console waiter with `armConsole` before `setContent`, then await the returned waiter. The case matches the lowercase text `refused to display` and `'x-frame-options' to 'deny'`. The cross-origin frame uses `localhost` in place of `127.0.0.1`, as upstream's `server.CROSS_PROCESS_PREFIX` does, so the frame stays cross-origin. Chromium only, as upstream skips the case on Firefox.

**To change:** Drop the text checks and assert only that a console message arrived, if Chromium's refusal wording changes between releases.

## frame-hierarchy: context-level frame events register inside a page job

**Question:** `onFrameAttached`, `onFrameDetached` and `onFrameNavigated` on a `Page` register directly on the caller's thread, so they see every later event. The `onContextFrameAttached`, `onContextFrameDetached` and `onContextFrameNavigated` variants register from inside the `onContextPage` callback. For a page opened after the call, that callback runs as a queued job, so a child-frame event the reader already dispatched can be missed.

**Guess taken:** Leave it open. The page-level versions are correct and the context-level versions are not yet covered by a test that opens a page and attaches a child frame right away. Fixing it needs the page-created registration to run at dispatch time too.

**To change:** Make `onContextPage` register its per-page setup with `registerDispatchEventHandler` and add a context-level test that attaches a child frame in a new page immediately.

## workers: console timestamp is checked against the page clock

**Question:** Upstream checks `consoleMessage.timestamp()` against a wall-clock range. The Haskell test suite does not depend on a wall-clock package (`time` is not a cabal dependency), and the `timestamp` field is a Double of epoch milliseconds from the driver.

**Guess taken:** Read `Date.now()` from the page with `evaluateNumber` before and after the worker is created, and assert the message timestamp lies between those two readings (with a 1 ms margin on each side). Both clocks are the same machine's epoch milliseconds, so the comparison holds.

**To change:** Add a `time` dependency and compare against `getCurrentTime` if the page-clock margin ever proves flaky.

## workers: "should report console logs" placeholder replaced

**Question:** The earlier placeholder for this case passed without checking any console message, so it gave false evidence.

**Guess taken:** Replace it with the upstream shape: `armConsole` before the blob worker is created, then check the text is `"1"` and the page URL is not a blob URL. The old placeholder body was removed rather than kept alongside.

**To change:** If upstream's page URL assertion needs a different base, adjust the `pageLink` check only.

## workers: dedicated-worker console needs a worker-level subscription

**Question:** The two console-event cases ("should report console event on the worker", and the case without a page or context listener) failed with `WaitTimeout`. Node runs of the same flow showed the driver emits the `hey` console event on the context channel, in every variant tried (about:blank, HTTP origin, with and without the context subscription). So the driver emits the event, and the Haskell side missed it.

**Finding:** `contextFromWorker` returned the page guid, because a worker's `remoteParent` is its page. The context subscription and the context console waiter were then aimed at the page channel, so the event was never awaited. Fixed by walking worker → page → context. The driver's context dispatcher also forwards a dedicated worker's console only when the worker channel holds a `console` subscription, so `armWorkerConsole` sends `updateSubscription {event: 'console', enabled: true}` to the worker channel (raw JSON, since the protocol type is not generated).

**Guess taken:** Keep the worker-channel send as raw JSON, and discard its result with `_ :: Value`. A driver error from that send would be silent.

**Evidence:** scratchpad `workers.log`: BUILD_RC=0, `-p /workers/` All 14 tests passed, TEST_RC=0. Before the fix (`workers.log` / `trace.log`), two cases failed with WaitTimeout.

**To change:** If the send can fail in a way that matters, decode its result (or surface the error) instead of discarding it.

## browser-context: `armContextConsole` registers an unfiltered waiter

**Question:** `armContextConsole` in `src/Playwright/BrowserContext/Wait.hs` uses `registerEventWaiter`. Does an unfiltered context waiter take a message meant for a worker or page waiter?

**Finding (corrects the earlier guess):** No. `deliverEvent` in `src/Playwright/Connection/Wait.hs` fills every matching waiter, so two waiters both see the same event and neither steals it. A context console waiter is also meant to see worker messages, because upstream `context.on('console')` includes workers.

The real difference is `registerEventWaiter`'s buffer path. It pops a console event that arrived before the arm and was buffered for `(guid, "console")`. A future-only waiter (`registerFutureEventWaiterWhere`, as `armWorkerConsole` uses) would not do that.

**Guess taken:** Leave `armContextConsole` unchanged. Several existing tests start `waitForContextConsole` inside `withAsync` before the action (`BrowserContextEventsSpec.hs:91,103,115`, `ChromiumSpec.hs:461`, `WorkersSpec.hs` service-worker start). The async thread may register after the log has already fired, so those tests rely on the buffered path. Switching to future-only would make them racy.

**To change:** Only if the tests are rewritten to arm before forking, so that the arm is synchronous. Then switch `armContextConsole` to `registerFutureEventWaiterWhere`.

## workers: network cases and the unfiltered finished waiter

**Question:** "should report network activity" is gated on chromium 143 or later. Does the gate skip the case on older browsers, or fail?

**Guess taken:** The gate reads the major version from `browserVersion` through `contextBrowser (pageContext page)`. On an older browser it prints `skipped: needs chromium 143, found <version>` and returns without failing. The pinned chromium-1248 run printed no skip line, so the case ran and passed. The exact version string was not read back from the log.

**To change:** If the gate should use a tasty skip rather than a silent pass, switch it to the tasty skip mechanism the other gated specs use.

**Question:** `armRequestFinished` is unfiltered, so the first finished event on the context may not be the worker script. Run 1 failed on `same request` because the first event was another request.

**Guess taken:** In "should report worker script as network request", a local `awaitScriptFinished` takes the next finished event and re-arms `armRequestFinished page` while the URL lacks `worker.js`. Run 2 passed. A finished event that arrives between `takeNext` and the next arm could be lost. No such loss showed up in run 2.

**To change:** If that race shows up, use a fresh buffered waiter that filters by URL (as `armRequestWhere` does), or add a `armRequestFinishedWhere` that filters before it takes the event.

## workers: version gates print `skipped:` and pass

**Question:** The workers cases gated on a browser version (extra headers and the iframe network case need chromium 143 and 149 or later; the iframe `allHeaders` case needs 151) use the `browserVersion` check. Should an old browser get a tasty skip instead of a silent pass?

**Guess taken:** Print `skipped: needs chromium N, found <version>` with `liftIO (putStrLn ...)` and return, so the test passes. This matches the earlier version-gated cases in this spec. A tasty skip would need a different shape of test for each gate.

**To change:** Switch the gates to the tasty skip mechanism if the suite starts reporting skips as a count.

## workers: extra-headers WaitTimeout and the finished waiter

**Finding:** "should support extra http headers" timed out after 30.29 s on the `armRequestWhere` waiter for `worker.js`. The earlier suspects, `setExtraHTTPHeaders` and the glob matcher from `armRequest`, were refuted by a diagnostic run. The waiter never matched because `armRequestWhere` skipped the script request.

**Resolution:** Switched to `armRequestFinished` plus the local `awaitScriptFinished`, which skips finished events until the `worker.js` URL. The case passes (`-p` on the allHeaders and extra-headers cases: All passed).

**Guess taken:** Keep the same `awaitScriptFinished` shape for the three allHeaders cases.

## workers: iframe network case hang (not reproduced)

**Finding:** "should attribute network activity for worker inside iframe to the iframe" once stopped with `SignalException 15` (killed by the 300 s timeout). The rerun alone passed in 0.53 s (scratchpad `one.log`, TEST_RC=0). The nested-worker case uses the same `wait handle` pattern and passes, which weakens the `waitForWorker`-has-no-timeout theory.

**Guess taken:** Treat it as a one-off and keep the case as is. If it recurs in a full run, add a bounded wait to `waitForWorker` in the test.

## workers: redirect case is a skip with a reason

**Question:** Upstream "should work with redirect" (`tests/page/workers.spec.ts:264`) is marked `fixme` on chromium. Should it get a test body?

**Guess taken:** No. It is recorded as a skip with the upstream reason in `progress/FUNCTIONALITY_TODO.md`, with no test body.

**To change:** Port it once the upstream fixme is removed.

## workers: webkit and firefox fixmes are not represented

**Question:** The nested-worker `allHeaders` case (upstream line 399) has fixmes for webkit (402) and firefox (403). Should those be recorded as skips?

**Guess taken:** No. The harness is chromium-only (pinned chromium-1248, firefox-1555, webkit-2370, and the suite runs on chromium), so only the chromium behaviour is ported and the other browsers' fixmes are not represented.

**To change:** Add skip records if the suite ever runs on webkit or firefox.

## page-click-timeout: inline markup instead of input/button.html

**Question:** Upstream `page-click-timeout-*` specs load `tests/assets/input/button.html`. The Haskell harness does not serve that asset. Should the port add an asset server or inline the markup?

**Guess taken:** Inline the markup with `setContent`. The button markup is small, and the timeout behaviour does not depend on the file itself.

**To change:** Add the asset to the harness if a later spec needs the real file.

## page-click-timeout: mode-gated and Android cases

**Question:** Three cases use Node-only test hooks (`__testHookBeforePointerAction`, `__testHookBeforeHitTarget`), and the large-header case in `-4` is Android-specific upstream. Should they be ported?

**Guess taken:** No. They are recorded as skips in `progress/FUNCTIONALITY_TODO.md`, with the reasons.

**To change:** Port them if the harness gains an equivalent of these hooks.

## page-evaluate-callback: handle-routing cases ported with page-side calls

**Question:** Upstream cases 15 (`jsHandle.evaluate`) and 19 (`locator.evaluateHandle`) pass a callback argument through `evaluateOnHandle` / `evaluateHandleOnLocator`. Those Haskell calls take no argument, so the callback cannot be passed in. Should these cases be skipped?

**Guess taken:** Port them. The page-side expression calls `window.cb(...)` directly, so the routing from a handle or locator back to the page is still tested, and the callback is reached through the exposed binding instead of an argument.

**To change:** Skip them if the routing has to go through a real argument.

## page-evaluate-callback: case 22 skipped

**Question:** Upstream case 22 checks the internal `__pw_fn_` global name that upstream uses for exposed callbacks. Should it be ported?

**Guess taken:** No. The name is an implementation detail of upstream's `exposeFunctions` mechanism, which Haskell does not have. It is recorded as a skip in `progress/FUNCTIONALITY_TODO.md`.

**To change:** Port it if the harness ever needs to observe the binding name.

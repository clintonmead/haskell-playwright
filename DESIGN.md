# Playwright Haskell

## Keep this document during compaction

Don't push this document out of your context during compaction. 

## Project Structure

This will be a Haskell project. It shall be buildable with both the latest [Stack LTS](https://www.stackage.org/lts-24.58) and [cabal](https://www.haskell.org/cabal/).

The project shall use [Nix](https://nixos.org/nix/) for dependency management. Create a [flake.nix](flake.nix) file for the project, and in that have a development shell with all the dependencies required for building the project, including [Stack](https://www.haskell.org/stack/) and [cabal](https://www.haskell.org/cabal/). The same shell puts GHC-matched `haskell-language-server-wrapper` on `PATH`. Cursor/VS Code must use `${workspaceFolder}/scripts/haskell-language-server-wrapper` (`haskell.serverExecutablePath` in `.vscode/settings.json`) because the GUI PATH does not include the flake shell; the script runs `nix develop --command haskell-language-server-wrapper`. `.envrc` is `use flake`, and `hie.yaml` pins a Cabal cradle so HLS does not pick Stack. After changing this, reload the Cursor window so the Haskell extension retries HLS.

Package metadata lives in `package.yaml` ([hpack](https://github.com/sol/hpack)). Stack regenerates the `.cabal` files on build; run `hpack` (and `hpack codegen`) before a Cabal-only build if those files are stale.

## Overall task

You are going to make Haskell bindings to [Playwright](https://github.com/microsoft/playwright). Whilst the [Playwright github](https://github.com/microsoft/playwright) itself is your primary source of information and structure, ports like [playwright-dotnet](https://github.com/microsoft/playwright-dotnet) may also give you ideas.

We don't just want to wrap and call Typescript code. The [protocol files may be handy](https://github.com/microsoft/playwright/tree/main/packages/protocol/spec).

### Local upstream code access

I've cloned the repository and you can locally access the upstream Typescript code for [Playwright](https://github.com/microsoft/playwright) code at `upstream-typescript-playwright` in a gitignored subdir in this repository.

## Code design

### Don't copy paste code

Do not copy-paste code. Nix is very powerful at getting files from external sources at build time. Use this. 

### I encourage you to do refactors even big ones if you think it will improve code quality

It's okay if you've think in hindsight you've designed something the wrong way and need a bit refactor. Indeed I encourage you to do these refactors as you go. Just close your current work of with a commit that passes, then stop and perform the refactor. Ensure all the tests pass, and then commit the refactor, and continue your work. Don't be afraid to make big changes either. The code quality at the end is more important than short term progress.

### Prefer immutability over mutability

Whilst one can have mutable state in Haskell with things like `TVar`s and `IORef`s, prefer immutability over mutability. Using `TVar`s and `IORef`s usually code smell so avoid these without asking me first.

### Don't put everything in IO

Generally one should wrap IO with custom monads. Putting everything in IO is also code smell.

### Keep Haskell files small

Keep Haskell files relatively small. Except for very trivial types, one data type per file is often a good idea, along with functions directly associated with that type. 

### Use strict warnings

Start with these warnings:
`-Werror` `-Weverything` `-Wno-all-missed-specialisations` `-Wno-implicit-prelude`
`-Wno-missing-safe-haskell-mode` `-Wno-prepositive-qualified-module`
`-Wno-safe` `-Wno-unsafe` `-Wno-missing-kind-signatures` `-Wno-missing-role-annotations`

If you think there is another warning that it would be better to ignore, ask me first. If I say they're safe to ignore you can add them to a `-Wno-` flag, but if I refuse you should then fix the warnings.

### Use types

Actually use types in Haskell, it's Haskell's strength. Do not just make everything a `String`, `Int` or `Bool`. Use `newtype` to describe types with particular meaning.

### Don't repeat yourself, always look for abstractions

Always look for abstractions to avoid repeative boilerplate code. 

### Use public packages/libraries where possible

Avoid reinventing the wheel. Look for public packages/libraries that can be used to implement the functionality you need. Prefer libraries over your own code.

Libraries we rely on rather than hand-rolling:

- `autodocodec` for JSON (`HasCodec` + `deriving via Autodocodec`; newtypes may use `deriving newtype (ToJSON, FromJSON)`)
- `yaml` for loading the protocol spec as JSON `Value`s
- `text-manipulate` for identifier casing in codegen
- `binary` for the driver's 4-byte little-endian frames
- `typed-process` for spawning `run-driver`
- `opt-env-conf` for the codegen CLI
- `stm` / `async` for the connection reader
- `mtl` / `unliftio` / `exceptions` for `Playwright a` (`ReaderT Connection IO`)
- `warp` / `wai` / `wai-app-static` for local test servers
- `tasty` / `hedgehog` for tests

### Use Autodocodec for JSON

Do not write `instance ToJSON` or `instance FromJSON`. Use [Autodocodec](https://hackage.haskell.org/package/autodocodec) (`HasCodec`) and obtain `ToJSON`/`FromJSON` with `deriving via Autodocodec`. Transparent newtypes may use `deriving newtype (ToJSON, FromJSON)` instead.

### Never generically derive instances

Don't generically derive instances without asking me first. You shouldn't have to if you use Autodocodec.

### READ THESE GUIDES

Read [LLMs Will Cheese Your Types: Fighting Back in Haskell](https://blog.jle.im/entry/llms-and-haskell-1-constraint-evading-behavior.html) and ["Five-Point Haskell": Unconditional Election (via Parametricity)](https://blog.jle.im/entry/five-point-haskell-part-2-unconditional-election.html) very closely and follow all it's advice on writing good Haskell code.

## Testing

Ideally, we want all the tests the official Playwright tests have. We'll naturally need to convert these to Haskell.

### Selecting tests

Tasty already filters tests with `-p` / `--pattern` (the same idea as Hspec `--match`). Use that after changing a spec; do not run the full suite after every edit.

In the Nix shell, extra `playwright-test` arguments are passed through to Tasty:

```
playwright-test -p /page-goto/
playwright-test -p '/page-goto/&&/should work/'
playwright-test --list-tests
```

Do not pass `-p` directly to `nix develop` (`-p` is a Nix flag):

```
nix develop --command bash -c 'playwright-test -p /page-goto/'
```

`TASTY_PATTERN` is Tasty's own environment-variable form of the same filter. Full-suite `playwright-test` (no extra args) is for a green commit.

### Test reliability

Do not put artificial delays in tests, particularly when those artificial delays are not in the official Playwright tests. That could result in flaky tests and unexpected runtime failures.

### Run individual tests most of the time to test new functionality

Most of the time, run individual tests to test new functionality. Occasionally run the full test suite, in particular before a commit. By default, tests should run in parallel. 

## Implementation process

### Go for it

You've already a good amount of progress now. Continue to completely implement the Playwright API and all tests that the official Playwright tests have. Only stop if you have a question or need to ask me for guidance.

### You can make commits

Make commits to git at appropriate milestones.

### Script things you run regularly

If you have non-trivial commands you run regularly, ensure they are scripted, either in `/scripts` or in `flake.nix`. This is so humans can easily reproduce your build and test runs. 

### Update your progress regularly

There are the files `progress/FUNCTIONALITY_DONE.md` and `progress/FUNCTIONALITY_TODO.md`. Update these files as you go, and also ensure they're up to date before commits.

## Reread this document and modify it

Reread this [document](DESIGN.md) occasionally so you don't forget about it's contents. Also modify it as you go. You can make notes here, and remove things which are think are no longer relevant.

## Notes

- User-facing browser automation lives in `Playwright a` (`ReaderT Connection IO`, with `MonadIO` / `MonadUnliftIO` / `MonadThrow`). `runPlaywright` starts the driver, connection reader, and serial handler thread. There is no root `data Playwright` instance; `chromium` / `firefox` / `webkit` are `Playwright BrowserType`. `Channel` is a guid plus phantom tag (no connection pointer); `send` reads the runtime from the monad. Session snapshot state stays in one `TVar Session` on `Connection`. Event/route/locator callbacks stored on Session remain `IO` and are wrapped with `runPlaywrightWith` at registration.
- Driver handshake sends `sdkLanguage: javascript` (`PW_LANG_NAME=javascript`) until the protocol grows a Haskell language tag. The driver process also gets `PWTEST_UNDER_TEST=1` so injected highlight UI uses an open shadow root, matching official Playwright tests.
- Chromium on NixOS needs `chromiumSandbox = false` at launch.
- Page navigation and title are Frame protocol methods; the public Page API delegates to `mainFrame`.
- `Browser.newPage` is a client helper (`newBrowserPage` / `newBrowserPageWith`: new context + new page). The page owns that context, so `closePage` closes the context (JS `_ownedContext`). A second `newPage` on that context is a client `DecodeFailure` (`Please use browser.newContext()`; JS `_ownerPage`). `browser.contexts()` is creation order tracked on the Connection, not guid-map order. `context.browser()` is cached at `newContext` so it still works after `close`. `browser.browserType()` (`browserBrowserType`) is cached at `launch`. `browser.version` (`browserVersion`) is `BrowserInitializer.version`. `browserType.name` / `executablePath` are `BrowserTypeInitializer` fields. `onBrowserContext` listens for Browser `context`. `onPageClose` listens for Page `close`. `closeBrowser` swallows `TargetClosedError` so a second close from `withBrowser` is a no-op. `newContextWith` rejects `deviceScaleFactor` / `isMobile` when `noDefaultViewport` is set (`viewport: null`). `clearCookiesWith` maps `ClearCookiesOptions` onto protocol `name` / `nameRegexSource` / `domain` / `path`.
- Locator is client-side (`Frame` + `Selector` + `strict: true`), not a channel object. Nested locators join with Playwright's ` >> ` combinator.
- Element actions (`fill`, `innerText`, …) are locator functions; Page owns navigation, `setContent`, `content`, screenshots.
- `deriving via Autodocodec` needs the `Autodocodec` constructor in scope (`import Autodocodec (Autodocodec (Autodocodec), ...)`).
- Transparent newtypes that appear in Autodocodec fields also `deriving newtype HasCodec`.
- Protocol floats that are not timeouts are `Scientific` (Autodocodec has no `HasCodec Double`).
- The test binary is linked `-threaded -with-rtsopts=-N`. Tasty’s default `--num-threads` is the number of RTS capabilities (CPU threads). Override with `-j` / `--num-threads` / `TASTY_NUM_THREADS`. Individual runs use Tasty `-p` (see Testing). Driver teardown ignores `waitForProcess` `ECHILD` (`does not exist (No child processes)`), which can still occur when many tests close Node drivers at once.
- Local packages are built with `-O0` (`optimization: False` in `cabal.project`, `$locals` / local `ghc-options` in Stack). Snapshot/Hackage dependencies stay optimized.
- Cabal `verbose: 0` plus the flake `cabal` wrapper (`-v0`) and Stack `cabal-verbosity: 0` keep successful builds quiet; GHC errors and test output still show. In the Nix shell, `playwright-test` (see `scripts/playwright-test`) builds once, runs the Tasty binary with `--hide-successes --hide-progress`, and strips the driver's host-requirements skip line. Extra args go to Tasty (`playwright-test -p/page-goto/`).
- Locator `evaluate` is `Frame.evalOnSelector` with `strict: true`. Page `evaluate` is `Frame.evaluateExpression` with `isFunction: true`.
- `Frame.navigated` updates the stored initializer URL and name so `page.url` and `frame.name` stay live.
- `getByRole`/`getByText`/`getByLabel`/`getByTestId` are client-side `internal:` selectors, matching Playwright's locatorUtils. `GetByRoleOptions` maps `checked`/`disabled`/`selected`/`expanded`/`includeHidden`/`level`/`name`/`pressed` onto `internal:role` attributes (`exact` applies to `name`). Regex `name` is still locator `role=…[name=/…/]` only.
- `Page.setViewportSize` generated `mkPageSetViewportSizeParamsViewportSize` takes height then width (required-field hash order).
- Mouse coordinates are `CssPixel` (`Scientific` on the wire). Protocol methods are `mouseClick` / `mouseMove` / `mouseDown` / `mouseUp` / `mouseWheel`.
- Protocol JSON field `source` is `Selector` only for drag-and-drop; init-script `source` is `Text`.
- `Frame.loadstate` events patch the stored initializer `loadStates` so `waitForLoadState` can STM-retry until the lifecycle is present.
- `page.$` is `querySelector` with `strict: false`; locator `elementHandle` waits attached then queries with `strict: true`. `page.$$` is `querySelectorAll`. `evalOnSelectorBool` is `$eval` decoded as `Bool`.
- Frame objects are created under BrowserContext on the wire. `childFrames` uses `FrameInitializer.parentFrame`, not the transport parent guid.
- ElementHandle `evaluateExpression` is the JSHandle mixin method on the same channel.
- Console and dialog events are BrowserContext protocol events; Page waits subscribe via `updateSubscription` then STM-wait on the context guid.
- `waitForURL` glob-matches the live frame URL (`**` / `*` / `?`) then waits for load.
- `waitForResponse` subscribes to BrowserContext `response` events and filters by URL glob.
- Popups arrive as BrowserContext `page` events; `opener` is `PageInitializer.opener`.
- BrowserContext always emits `page` (not behind `updateSubscription`). The first `newPage` is also buffered, so `waitForPopup` skips pages whose opener is not the waiting page.
- Cookies use `BrowserContext.addCookies` / `cookies` / `clearCookies`. Session cookies have expires `-1`. Chromium reports `document.cookie` SameSite as Lax. `clearCookiesWith` sends `nameRegexFlags` (empty) whenever `nameRegexSource` is set; the driver ignores a regex source without flags.
- `Page.emulateMedia({})` omits fields (keeps the previous override); `media: no-override` resets.
- Request events require `BrowserContext.updateSubscription` for `request`, same as `response`.
- `evaluateHandle` is `Frame.evaluateExpressionHandle`. `asElement` checks `ChannelTypeElementHandle`.
- Web storage is `Page.webStorage*` with `kind: local|session`.
- Workers are Page-channel objects (`worker` events are not behind `updateSubscription`). `workers` omits channels that have emitted `close`.
- `JSHandle.getProperty` / `getPropertyList` wrap the JSHandle mixin. `jsonValueSpecial` / `jsonValueTexts` decode specials and string arrays.
- `Frame.dragAndDrop` is exposed on locators. Official drag tests also use hover + mouse down/up. Locator `dispatchEvent` sends `eventInit` as `undefined`; `dispatchEventWith` / `dispatchEventHandleWith` take a `SerializedArgument` (`objectArgument`, `objectHandleArgument` / `jsHandleObjectArgument` for JSHandle fields such as `DataTransfer`). Nested objects and arrays use `objectValue` / `arrayValue`. `mkFrameDispatchEventParams` takes eventInit, selector, timeout, type; `mkElementHandleDispatchEventParams` takes eventInit then type (no timeout).
- ElementHandle `type` and `scrollIntoViewIfNeeded` take the same default timeout as other actions; `scrollIntoViewIfNeededWith` exposes the protocol timeout. A timeout waiting for visibility is reported as `Timeout <n>ms exceeded` on the protocol (the JS client adds extra "element is not visible" wrapping).
- Persistent `registerEventHandler` callbacks run on a serial worker thread (not the connection reader) so they can `send` without deadlocking, and so binding/console/clock stubs observe dispatch order like Node's single-threaded client. After a command response, `sendCommand` waits for those dispatched handlers to finish so side effects (e.g. `exposeFunction` stubs) are visible when the triggering call returns. A handler that itself calls `send` skips that wait to avoid deadlock. `route` handlers are not counted: they can block indefinitely, and `page.close` / `context.close` must not wait for them (JS yields on the Promise). `exposeFunction` is `Page.exposeBinding` plus a `bindingCall` handler that `resolve`/`reject`s the BindingCall. `contextExposeFunction` is the same on the BrowserContext channel. `addContextInitScript` is `BrowserContext.addInitScript` and returns a `Disposable`.
- `Browser.newContext` options are a curated `NewContextOptions` record mapped onto generated `BrowserNewContextParams` (`newContextWith`). Viewport `mk*` still takes height then width; HTTP credentials `mk*` take password then username. `deviceScaleFactor` is a `Scientific` newtype. Runtime twins: `setGeolocation` (including accuracy), `setOffline`, `grantPermissions`, `setContextExtraHTTPHeaders`, `setHTTPCredentials`. `page.setExtraHTTPHeaders` overrides matching context header names. `newContextWith` also injects any `registerSelector` engines stored on the Connection as `selectorEngines`. `proxy` (`ProxySettings`) is on both `NewContextOptions` and `LaunchOptions` (and persistent launch).
- `BrowserType.launchPersistentContext` (`launchPersistentContext`) sends protocol `launchPersistentContext` with launch + context options and `userDataDir` (resolved with `makeAbsolute`). The default page is `context.pages()[0]`. Close the **context** (the server tears down the owned browser). `context.browser()` is cached at launch like `newContext`. Selector engines stored on the Connection are injected as `selectorEngines`. Launch `downloadsPath` is on `LaunchOptions` (made absolute before send) and is also mapped on persistent launch. Launch `executablePath` is sent as given (not resolved). `onContextClose` is `registerEventHandler` on context `close`.
- `registerSelector` / `registerSelectorWith` store engines on the Connection, send `BrowserContext.registerSelectorEngine` to existing non-closed contexts, and inject them on later `newContext` calls. Source is `Text` (callers wrap functions as IIFEs; `{ path }` is file contents). Client-side name checks match the driver (charset `[a-zA-Z0-9_-]`, predefined engines, duplicates) with JS `selectors.register:` prefixes. `contentScript: true` runs the engine in the isolated world.
- `waitForNavigation` waits for the next `Frame.navigated` (not a buffered prior navigation), optionally glob-filtered, then load, then `Request.response` for `newDocument.request`.
- JSHandle `evaluateExpression` uses `isFunction: true` for function source and `false` for raw expressions.
- `fileChooser` is a Page `updateSubscription` event (not BrowserContext). `setInputFiles` sends in-memory `payloads` (name + base64 buffer), not host `localPaths`.
- `page.route` registers handlers on the connection keyed by page guid, then `Page.setNetworkInterceptionPatterns`. `BrowserContext.route` (`contextRoute`) uses the context guid and `BrowserContext.setNetworkInterceptionPatterns`. The driver emits a single `route` event on page or context; page dispatch then tries context handlers so page `fallback` can yield to context routes. `fallback` does not continue the network request; it tries the next matching handler (newest first), re-checking globs against the live request URL after overrides. If none handle the route, the driver `continue`s with `isFallback: true`. Fallback/continue overrides (headers, method, URL, post data) stick on the request guid so later handlers see them. `routeTimes` expires a handler after N invocations. `fulfill` sends UTF-8 text or base64 bytes plus Content-Length / Content-Type. `unrouteAll` / `unrouteContextAll` drop every handler for that guid and refresh interception patterns (default behavior; wait/ignoreErrors are not implemented yet). `continue` / `fulfill` / `abort` swallow `TargetClosedError` like the JS client. If the route object is already gone (`MissingObject`), continue/fallback skip request overrides.
- `contextPages` lists Page channels whose transport parent is the context guid and that have not emitted `close`. `waitForContextPage` uses a future `page` waiter so a buffered first `newPage` is not consumed, and races a `close` waiter so context close aborts with `TargetClosedError` (`Target page, context or browser has been closed`). `onContextPage` is `registerEventHandler` on `page` (do not assert inside the handler; exceptions are swallowed). `waitForContextClose` returns the same context. `closeContext` a second time swallows `TargetClosedError` via `ignoreTargetClosed`. `page.context` is `pageContext`; compare contexts with `contextsEqual`.
- Connection runtime is split: `Connection.Types` holds the record (`Transport` + `TVar Session` + handler `TQueue`), `Connection.Store` the guid maps, `Connection.Wait` event waiters, `Connection.Handlers` the serial handler queue. `Page.Wait` holds page `waitFor*` / `arm*` helpers (same pattern as `Page.Query` / `Page.Network`). `Route.Intercept` holds page/context `route` / `unroute` / dispatch; `Route` is the channel object (`continue` / `fulfill` / `abort` / `fetch`). `browser.contexts()` is creation order on Session, not guid-map order.
- `NewContextOptions` `baseURL` is sent as protocol `baseURL` and stored on the connection keyed by context guid. `Frame.goto` resolves relative URLs against that store (absolute/`data:`/`about:`/`blob:` URLs are unchanged). Path-absolute URLs keep the base origin; other relatives replace the last path segment (WHATWG-ish). Do not decode the full `BrowserContextInitializer` for this: required `debugger` is a channel that may be absent from the stored JSON. `waitForURL`/`route` globs are not yet resolved against baseURL.
- `page.clock.*` is a client helper on the BrowserContext channel (`clockInstall`, `clockPauseAt`, `clockRunFor`, `clockFastForward`, `clockResume`, `clockSetFixedTime`, `clockSetSystemTime`). Times/ticks are `timeNumber`/`ticksNumber` (`Scientific`) or `timeString`/`ticksString`.
- `storageState` / `setStorageState` are BrowserContext protocol methods. Create-time `NewContextOptions` `storageState` maps onto `BrowserNewContextParams.storageState`. IndexedDB and WebAuthn credentials are not in the Haskell `StorageState` type yet.
- `NewContextOptions` also maps `hasTouch`, `isMobile`, and `noDefaultViewport`.
- Page `download` events are not behind `updateSubscription`. `waitForDownload` waits on the page guid; `downloadPath` is Artifact `pathAfterFinished`. `downloadSaveAs` is Artifact `saveAs` (path is made absolute). `downloadDelete` / `downloadCancel` / `downloadFailure` are the matching Artifact commands. `acceptDownloads` is `Accept`/`Deny` on `NewContextOptions`.
- `JSHandle.toString` is the stored initializer `preview`. `previewUpdated` patches that field like `Frame.navigated` patches URL. `waitUntilJsHandlePreview` STM-retries until the preview matches (document.body's HTML preview arrives asynchronously).
- `evaluateHandleExpression` is `evaluateExpressionHandle` with `isFunction: false` (raw expressions such as `'12'`).
- `Frame.frameElement` returns the iframe/frame `ElementHandle`. Detached frames fail with `Frame has been detached.`
- `page.pdf` is Chromium `Page.pdf` and returns PDF bytes. `PdfOptions` currently maps `tagged` and `outline`.
- `goto` always sends `waitUntil` (default `load`), matching the JS client.
- Locator `dropOnto` is Frame `drop` with in-memory `payloads` and clipboard-like `data`. Haskell uses `dropOnto` because Prelude `drop` is already taken. Host `localPaths` are not sent yet.
- `page.$$` is `querySelectorAll` (`Frame.querySelectorAll` on the main frame). `evalOnSelectorBool` is `$eval` decoded as `Bool`. `waitForFrameNavigated` / `armFrameNavigated` wait for the next main-frame `navigated` event without waiting for load (JS `waitForEvent('framenavigated')`). `waitForLoadEvent` / `armLoadEvent` wait for main-frame `loadstate` add=`load` without consuming buffered events (`registerFutureEventWaiter`). `waitForFrameDetached` is Page `frameDetached`.
- `responseFrame` is `request.frame` of the response's request.
- Page `onLoad` listens for the main frame `loadstate` add=`load` event. Official page-event-load uses a 1000ms `waitForTimeout` after goto to check the handler did not double-fire.
- ElementHandle `$eval` / `$$eval` are `evalOnSelector` / `evalOnSelectorAll` with `isFunction: true`. Page `$eval` and ElementHandle `$eval` omit `strict` (first match), matching the JS client; locator `evaluate` still sends `strict: true`.
- ElementHandle `$` / `$$` are `querySelector` / `querySelectorAll` on the handle. Hover/fill/check/focus/selectOption/dispose are ElementHandle mixin methods. A second `dispose` is a no-op: the driver reports `TargetClosedError` after the handle is gone.
- Page `getAttribute`/`innerText`/`innerHTML`/`inputValue`/`textContent`/`isEnabled`/`isChecked`/`isVisible`/`isHidden` and friends take a selector and call the same Frame methods locators use (`strict: true`). `pageIsVisible` / `pageIsHidden` have no timeout, matching the JS client. ElementHandle `isEditable` is the mixin getter (`isEditableHandle`).
- Locator `describe` appends `internal:describe=` plus JSON; `description` reads that suffix only when it is last. `locator.page()` is reconstructed from channels stored on the Locator. `filter({ visible })` appends `visible=true|false`. `filterHasText` / `filterHasNotText` append `internal:has-text=` / `internal:has-not-text=` plus `escapeForTextSelector` (string `hasText` is inexact). `filterHasTextRegex` takes a `/pattern/flags` source. `filterHas` / `filterHasNot` / `andLocator` / `orLocator` append `internal:has=` / `internal:has-not=` / `internal:and=` / `internal:or=` plus JSON of the inner selector. Chaining a locator uses `internal:chain=`.
- Click `modifiers` is `Maybe [KeyboardModifier]`: `Nothing` omits the protocol field (keeps keyboard state); `Just []` sends an empty list and clears modifiers for that click. `clickOptionsButton` / `clickOptionsClickCount` / `clickOptionsTrial` / `clickOptionsSteps` map onto Frame/ElementHandle `click`. `closePageReason` sets Page/context `close` `reason`.
- `NewContextOptions` / persistent launch / `emulateMedia` map `reducedMotion`, `forcedColors`, and `contrast`. `emulateMedia` `Nothing` omits a field (leaves the previous value); `NoOverride` restores the default.
- `waitForRequest` / `waitForResponse` / `waitForDownload` abort when the page guid is closed (not only when the whole connection dies). `responseFinished` fails with target-closed if the request object is disposed before it settles.
- `requestAllHeaders` is protocol `Request.rawRequestHeaders`.
- Locator `blur` is Frame.blur. `allTextContents` / `allInnerTexts` are `evalOnSelectorAll`. Bounding box / scrollIntoView / selectText resolve an element handle, run the mixin, then dispose it.
- Void `waitForSelector` sends `omitReturnValue: true`. `waitForSelectorHandle` / `waitForSelectorOnHandle` omit that flag so the driver returns an `ElementHandle`. Page `$`/`waitForSelector` is not locator-strict. Official page-wait-for-selector shadow-DOM uses a 100ms `waitForTimeout` before appending the span.
- `setDefaultTimeout` / `setDefaultNavigationTimeout` are client-side (page overrides context). Unset `GotoOptions` timeout uses the navigation timeout. Timeout `0` disables client-side waiters.
- `Request.sizes` loads `Request.response` then calls protocol `Response.sizes`. Missing response: `Unable to fetch sizes for failed request`. Warp chunked responses inflate Chromium `encodedDataLength`; tests that assert exact `responseBodySize` set `Content-Length` (Node's `res.end` does this). Official `empty.html` is 0 bytes.
- `request.timing` (`requestTiming`) copies `ResponseInitializer.timing` onto the request and sets `responseEnd` from `requestFinished` / `requestFailed` `responseEndTiming` (and fills `responseStart` when it is still `-1`).
- `FileChooser.page` (`fileChooserPage`) is the page that armed `waitForFileChooser`.
- `requestRedirectedFrom` is `RequestInitializer.redirectedFrom`. `requestRedirectedTo` is the Request whose initializer points at this one (the JS client stores `_redirectedTo` when that object is created).
- `waitForRequestFailed` / `waitForRequestFinished` subscribe to the matching BrowserContext events. `onRequest` / `onResponse` / `onRequestFailed` / `onRequestFinished` do the same and filter by the event's page. `requestFailure` is the `failureText` from `requestFailed`. `responseFinished` STM-waits until that request has `requestFinished` or `requestFailed` (navigation requests are always reported, so goto's response settles without an extra subscription). If the request object is disposed first (page closed mid-body), it fails with target-closed.
- `page.requests` is protocol `Page.requests` (the driver keeps the last 100).
- `requestPostDataJSON` parses `requestPostData` with Aeson; missing body is `Nothing`, invalid JSON is `DecodeFailure` (`POST data is not a valid JSON object: …`).

# Playwright Haskell

## Keep this document during compaction

Don't push this document out of your context during compaction. 

## Project Structure

This will be a Haskell project. It shall be buildable with both the latest [Stack LTS](https://www.stackage.org/lts-24.58) and [cabal](https://www.haskell.org/cabal/).

The project shall use [Nix](https://nixos.org/nix/) for dependency management. Create a [flake.nix](flake.nix) file for the project, and in that have a development shell with all the dependencies required for building the project, including [Stack](https://www.haskell.org/stack/) and [cabal](https://www.haskell.org/cabal/).

Package metadata lives in `package.yaml` ([hpack](https://github.com/sol/hpack)). Stack regenerates the `.cabal` files on build; run `hpack` (and `hpack codegen`) before a Cabal-only build if those files are stale.

## Overall task

You are going to make Haskell bindings to [Playwright](https://github.com/microsoft/playwright). Whilst the [Playwright github](https://github.com/microsoft/playwright) itself is your primary source of information and structure, ports like [playwright-dotnet](https://github.com/microsoft/playwright-dotnet) may also give you ideas.

We don't just want to wrap and call Typescript code. The [protocol files may be handy](https://github.com/microsoft/playwright/tree/main/packages/protocol/spec).

## Code design

### Don't copy paste code

Do not copy-paste code. Nix is very powerful at getting files from external sources at build time. Use this. 

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

## Implementation process

### Getting there slowly

Playwright is a big project. Perhaps get a minimum viable product working first, that is well tested, and then add more features gradually.

### You can make commits

Make commits to git at appropriate milestones.

## Reread this document and modify it

Reread this [document](DESIGN.md) occasionally so you don't forget about it's contents. Also modify it as you go. You can make notes here, and remove things which are think are no longer relevant.

## Notes

- Driver handshake sends `sdkLanguage: javascript` (`PW_LANG_NAME=javascript`) until the protocol grows a Haskell language tag.
- Chromium on NixOS needs `chromiumSandbox = false` at launch.
- Page navigation and title are Frame protocol methods; the public Page API delegates to `mainFrame`.
- `Browser.newPage` is a client helper (new context + new page), not a protocol method.
- Locator is client-side (`Frame` + `Selector` + `strict: true`), not a channel object. Nested locators join with Playwright's ` >> ` combinator.
- Element actions (`fill`, `innerText`, …) are locator functions; Page owns navigation, `setContent`, `content`, screenshots.
- `deriving via Autodocodec` needs the `Autodocodec` constructor in scope (`import Autodocodec (Autodocodec (Autodocodec), ...)`).
- Transparent newtypes that appear in Autodocodec fields also `deriving newtype HasCodec`.
- Protocol floats that are not timeouts are `Scientific` (Autodocodec has no `HasCodec Double`).
- Tests run with `NumThreads 1`: concurrent `waitForProcess` races on SIGCHLD.
- Local packages are built with `-O0` (`optimization: False` in `cabal.project`, `$locals` / local `ghc-options` in Stack). Snapshot/Hackage dependencies stay optimized.
- Cabal `verbose: 0` plus the flake `cabal` wrapper (`-v0`) and Stack `cabal-verbosity: 0` keep successful builds quiet; GHC errors and test output still show.
- Locator `evaluate` is `Frame.evalOnSelector` with `strict: true`. Page `evaluate` is `Frame.evaluateExpression` with `isFunction: true`.
- `Frame.navigated` updates the stored initializer URL and name so `page.url` and `frame.name` stay live.
- `getByRole`/`getByText`/`getByLabel`/`getByTestId` are client-side `internal:` selectors, matching Playwright's locatorUtils.
- `Page.setViewportSize` generated `mkPageSetViewportSizeParamsViewportSize` takes height then width (required-field hash order).
- Mouse coordinates are `CssPixel` (`Scientific` on the wire). Protocol methods are `mouseClick` / `mouseMove` / `mouseDown` / `mouseUp` / `mouseWheel`.
- Protocol JSON field `source` is `Selector` only for drag-and-drop; init-script `source` is `Text`.
- `Frame.loadstate` events patch the stored initializer `loadStates` so `waitForLoadState` can STM-retry until the lifecycle is present.
- `page.$` is `querySelector` with `strict: false`; locator `elementHandle` waits attached then queries with `strict: true`.
- Frame objects are created under BrowserContext on the wire. `childFrames` uses `FrameInitializer.parentFrame`, not the transport parent guid.
- ElementHandle `evaluateExpression` is the JSHandle mixin method on the same channel.
- Console and dialog events are BrowserContext protocol events; Page waits subscribe via `updateSubscription` then STM-wait on the context guid.
- `waitForURL` glob-matches the live frame URL (`**` / `*` / `?`) then waits for load.

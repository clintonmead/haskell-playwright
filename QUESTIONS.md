# Questions for the user

Decisions made without waiting for an answer. Each entry gives the question, the guess that was taken, and what to change if the answer is different. Newest first.

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

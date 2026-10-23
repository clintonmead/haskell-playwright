# Testing

Read by heading when running or writing tests. Test commands also live in `scripts/`.

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

`TASTY_PATTERN` is Tasty's own environment-variable form of the same filter. Full-suite `playwright-test` (no extra args) is periodic, not per commit: see the test budget under "Advice for working unattended".

### Headed demo

`playwright-test-headed` runs the same Tasty suite with a visible Chromium window, one test at a time, and `slowMo` so clicks and typing are watchable. It needs `DISPLAY` or `WAYLAND_DISPLAY`. Launch reads `PLAYWRIGHT_HEADED=1` (headless false) and optional `PLAYWRIGHT_SLOWMO` (milliseconds). Default headed Chromium args include `--disable-gpu` and `--ozone-platform=x11` when `DISPLAY` is set (`PLAYWRIGHT_HEADED_GPU=1` skips `--disable-gpu`).

```
playwright-test-headed
playwright-test-headed --demo
playwright-test-headed -p /page-click/
nix develop --command bash -c 'playwright-test-headed --demo'
```

`--demo` is a visual subset (goto, click, fill, drag, keyboard, dialogs, …). With no extra args the full suite runs headed. Do not use the quiet `playwright-test` wrapper for this: it hides successes and captures output until exit.

### Test reliability

Do not put artificial delays in tests, particularly when those artificial delays are not in the official Playwright tests. That could result in flaky tests and unexpected runtime failures.

### Run individual tests most of the time to test new functionality

Most of the time, run individual tests to test new functionality. Occasionally run the full test suite. You don't have to run the full test suite before the commit. By default, tests should run in parallel. You should be spending no more than 20% of your time running the full test suite.

Keep track of how long full tests runs are running and how frequently you're running them. If they're taking more than 20% of the time, run them less frequently and focus on running them only after significant refactors.

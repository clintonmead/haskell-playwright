# Resume

Short state file for picking up after compaction. Read this instead of rereading whole files.

## Post-compaction reads

1. `CLAUDE.md` in full.
2. `MEMORY.md` in full.
3. `DESIGN.md` in full. It is the short index. Topic files in `progress/design/` are read by heading only (`grep -n '^#' progress/design/<file>.md`).
4. `progress/FUNCTIONALITY_TODO.md`: `grep -n '^## '` for the sections, then the open items in the section you need. Ported `tests/page` items are short lines. Their full text is in `progress/TODO-PORTED-PAGE.md` (grep only: `grep -n -A3 '<a id="slug">'`). Long `tests/library` notes are in `progress/TODO-LIBRARY-NOTES.md` (grep only).
5. `QUESTIONS.md`: a numbered index, newest first. `head -40` is enough. Full entries are in `progress/QUESTIONS-ARCHIVE.md` (grep only: `grep -n 'id="q-N"'`).

Grep only, never read in full after a compaction: `progress/DESIGN-NOTES.md`, `progress/TODO-PORTED-PAGE.md`, `progress/TODO-LIBRARY-NOTES.md`, `progress/QUESTIONS-ARCHIVE.md`, `progress/FUNCTIONALITY_DONE.md`. Full rereads of these caused the autocompact thrash.

## Current state

- Next work: the remaining `tests/page` cases, then `tests/library`, then browsers. Run `scripts/upstream-gaps page` (it hides fully ported specs and prints a total; `ALL=1` shows them all).
- Last commit: `5d198e2` (browsercontext-events).
- Uncommitted and passing, all in `test/Playwright/`: PageNetworkRequestSpec (18 of 18), PageNetworkResponseSpec (21 of 21), PageHistorySpec (16 of 16), PageEventLoadSpec (2 of 2), PageRequestFallbackSpec (11 of 14, async gap q-85), PageEventRequestSpec (15 of 15), PageNetworkPostDataSpec (4 of 6, two `it.fail` Chromium cases recorded as skips), PageWaitForFunctionSpec (25 of 25), PageWaitForRequestSpec and PageSetExtraHTTPHeadersSpec (complete except recorded skips). Per-case detail is in the TODO and `TODO-PORTED-PAGE.md`.
- Commit blocker: these specs run against the uncommitted src tree, not HEAD. HEAD does not build (15 missing exports, `QUESTIONS-ARCHIVE.md` q-1). The guess is one catch-up commit of the verified tree. It has not been made, and q-1 is the user's call.
- Uncommitted progress reorganisation, do not commit unless asked: the TODO's Ported section is now a label index, long library items moved to `TODO-LIBRARY-NOTES.md`, `DESIGN.md` split into `progress/design/`, and `scripts/upstream-gaps` hides fully ported specs by default. The earlier doc split (`DESIGN-NOTES.md`, `QUESTIONS-ARCHIVE.md`, `TODO-PORTED-PAGE.md`) is also uncommitted.
- Commits in this session: `889feff`, `5d198e2`. Do not amend or push.

## Running tests

- Use the scratch slice: `slice-run.sh <group>` in the scratchpad. First copy `src/.`, `test/.`, `package.yaml` and `playwright.cabal` into the slice.
- Start it with `nohup` in the background. It writes `BUILD_RC=` and `TEST_RC=` to `scratchpad/slice-run.log`.
- Read the log tail when the result is needed. Never wait on a run.
- Build with `nix develop --command`. The inherited shell env pins an old driver.

## Next steps

1. Commit, once q-1 is answered: the verified tree in area groups, with explicit paths and the Haiku trailer. Do not amend or push.
2. Finish the remaining `tests/page` items from `scripts/upstream-gaps page`.
3. Then `tests/library`.

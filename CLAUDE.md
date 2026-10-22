# Playwright Haskell

Project brief and conventions live in @DESIGN.md. Read it first, and reread it after any compaction.

Progress: `progress/FUNCTIONALITY_TODO.md` (gaps and skips) and `progress/FUNCTIONALITY_DONE.md`.
Upstream gap report: `scripts/upstream-gaps page` (or `library`, or `SHOW=1 scripts/upstream-gaps page page-drag`).

## Keep working until the task is done

Do not end a turn to report progress or to ask the user which task to do next. The user has asked for this many times. Work the next item in `progress/FUNCTIONALITY_TODO.md` yourself. When a choice needs the user, make the best guess, carry on, and record the question and the guess in `QUESTIONS.md`. End a turn only when the user's whole request is finished, or when every remaining step needs an input that only the user can give.

## Do not wait on builds or test runs

Never block on a build or test run. Do not use Monitor with an `until` loop, a foreground `sleep`, or any command that waits for a background run to finish. Start the run in the background, keep editing the next item, and check its log with a quick read of the file's tail when you need the result. Read the result in the turn where it lands, and do not stop the turn while a run is still going.

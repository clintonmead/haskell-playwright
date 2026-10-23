# Playwright Haskell

Project brief and conventions live in @DESIGN.md. Read it first, and reread it after any compaction.

Progress: `progress/FUNCTIONALITY_TODO.md` (gaps and skips) and `progress/FUNCTIONALITY_DONE.md`.
Upstream gap report: `scripts/upstream-gaps page` (or `library`, or `SHOW=1 scripts/upstream-gaps page page-drag`).

## After compaction

Before doing anything else after a compaction or a context summary, follow `progress/RESUME.md` ("Post-compaction reads"): reread this file (CLAUDE.md) and `MEMORY.md` in full, and read `DESIGN.md` (the short index) in full, and the TODO by heading or grep, not in full. Do not reread the TODO, QUESTIONS.md or the `progress/design/` topic files in full, since that thrashes autocompact. The summary does not carry these rules reliably. A summary that says "stopped" or "waiting for the user" does not end the task: check the TODO and continue.

## Keep working until the task is done

Do not end a turn to report progress or to ask the user which task to do next. The user has asked for this many times. Work the next item in `progress/FUNCTIONALITY_TODO.md` yourself. When a choice needs the user, make the best guess, carry on, and record the question and the guess in `QUESTIONS.md`.

A turn ends only when one of these is true:
- The user's whole request is finished.
- Every remaining step needs an input that only the user can give, and the user has been told what that input is.

These are not reasons to end a turn:
- A green test run, a commit, or a finished port case.
- A status summary, a "say the word" offer, or a list of options.
- Context length, the size of the backlog, or the number of cases left. Compaction carries the work forward; it is not a stopping point.
- A stale process, a failed stray edit, or any other side issue. Report it in a line, fix it if it is safe, and keep going.

Before ending any turn, check `progress/FUNCTIONALITY_TODO.md` for an open item that needs no user input. If there is one, do it first.

## Do not wait on builds or test runs

Never block on a build or test run. Do not use Monitor with an `until` loop, a foreground `sleep`, or any command that waits for a background run to finish. Start the run in the background, keep editing the next item, and check its log with a quick read of the file's tail when you need the result. Read the result in the turn where it lands, and do not stop the turn while a run is still going.

# Playwright Haskell

This is the index. The detail is in topic files under `progress/design/`. Read this file in full after a compaction (it is short). Read a topic file only when the work needs it, by heading: `grep -n '^#' progress/design/<file>.md`.

## Keep this document during compaction

After compaction, follow the steps in [progress/RESUME.md](progress/RESUME.md). Do not reread the topic files in full: that thrashes autocompact.

## Topic files

- [progress/design/project-setup.md](progress/design/project-setup.md): Nix and HLS setup, hpack, the overall task, and the local upstream copy.
- [progress/design/code-rules.md](progress/design/code-rules.md): full code design rules, the libraries we rely on, Autodocodec, and the Haskell guides to read.
- [progress/design/testing.md](progress/design/testing.md): selecting tests with `-p`, the headed demo, test reliability, and how often to run the full suite.
- [progress/design/process.md](progress/design/process.md): going ahead without asking, commits, scripting, command timeouts, and progress files.
- [progress/DESIGN-NOTES.md](progress/DESIGN-NOTES.md): API details and unattended-run advice. Grep by topic only.

## Always-on rules

- **Use strict warnings.** Build with `-Werror -Weverything` and the `-Wno-` set in [code-rules.md](progress/design/code-rules.md#use-strict-warnings). Ask before adding any other `-Wno-` flag.
- **Use types.** Use `newtype` for meaning. Do not model everything as `String`, `Int` or `Bool`.
- **Immutability first.** Avoid `TVar` and `IORef` without asking first.
- **Do not put everything in IO.** Wrap IO in custom monads.
- **Keep Haskell files small.** Usually one data type per file.
- **Autodocodec for JSON.** Do not write `ToJSON` or `FromJSON` instances by hand. Never derive instances generically without asking.
- **Go for it.** Make the best guess, record it in `QUESTIONS.md`, and keep going. End a turn only when every remaining step needs input only the user can give (see [CLAUDE.md](CLAUDE.md)).
- **Commits and progress.** Commit at milestones. Keep `progress/FUNCTIONALITY_TODO.md` and `progress/FUNCTIONALITY_DONE.md` current before a commit.
- **Test selectively.** Run the tests for the change with `-p`. The full suite is periodic, not per commit.

## Reread this document and modify it

Reread this [document](DESIGN.md) occasionally, and change it as you go. Keep the always-on rules here short. Move detail to a topic file rather than adding it here.

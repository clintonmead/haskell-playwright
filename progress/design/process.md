# Implementation process

Read by heading when committing or updating progress files.

## Implementation process

### Go for it

You've already a good amount of progress now. Continue to completely implement the Playwright API and all tests that the official Playwright tests have. Do not stop to ask questions: make the best guess, record it in `QUESTIONS.md`, and keep going. Only end a turn when the work is finished or every remaining step needs an input only I can give (see the rules in CLAUDE.md).

### You can make commits

Make commits to git at appropriate milestones.

### Script things you run regularly

If you have non-trivial commands you run regularly, ensure they are scripted, either in `/scripts` or in `flake.nix`. This is so humans can easily reproduce your build and test runs. 

### Do not run commands with long timeouts

You shouldn't be running commands with timeouts greater than a minute, unless you're sure that command correctly running should take that time. Instead you should either:

a. Monitor the output of the command live AND/OR
b. Run a shorter timeout out, and confirm that the process was actually doing something that was interuptted before using a longer timeout.

### Update your progress regularly

There are the files `progress/FUNCTIONALITY_DONE.md` and `progress/FUNCTIONALITY_TODO.md`. Update these files as you go, and also ensure they're up to date before commits.

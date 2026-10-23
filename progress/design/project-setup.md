# Project setup and overall task

Read by grep or by heading. Linked from [DESIGN.md](../../DESIGN.md).

## Project Structure

This will be a Haskell project. It shall be buildable with both the latest [Stack LTS](https://www.stackage.org/lts-24.58) and [cabal](https://www.haskell.org/cabal/).

The project shall use [Nix](https://nixos.org/nix/) for dependency management. Create a [flake.nix](flake.nix) file for the project, and in that have a development shell with all the dependencies required for building the project, including [Stack](https://www.haskell.org/stack/) and [cabal](https://www.haskell.org/cabal/). The same shell puts GHC-matched `haskell-language-server-wrapper` on `PATH`. Cursor/VS Code must use `${workspaceFolder}/scripts/haskell-language-server-wrapper` (`haskell.serverExecutablePath` in `.vscode/settings.json`) because the GUI PATH does not include the flake shell; the script runs `nix develop --command haskell-language-server-wrapper`. `.envrc` is `use flake`, and `hie.yaml` pins a Cabal cradle so HLS does not pick Stack. After changing this, reload the Cursor window so the Haskell extension retries HLS.

Package metadata lives in `package.yaml` ([hpack](https://github.com/sol/hpack)). Stack regenerates the `.cabal` files on build; run `hpack` (and `hpack codegen`) before a Cabal-only build if those files are stale.

## Overall task

You are going to make Haskell bindings to [Playwright](https://github.com/microsoft/playwright). Whilst the [Playwright github](https://github.com/microsoft/playwright) itself is your primary source of information and structure, ports like [playwright-dotnet](https://github.com/microsoft/playwright-dotnet) may also give you ideas.

We don't just want to wrap and call Typescript code. The [protocol files may be handy](https://github.com/microsoft/playwright/tree/main/packages/protocol/spec).

### Local upstream code access

I've cloned the repository and you can locally access the upstream Typescript code for [Playwright](https://github.com/microsoft/playwright) code at `upstream-typescript-playwright` in a gitignored subdir in this repository.

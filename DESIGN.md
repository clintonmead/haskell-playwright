# Playwright Haskell

## Keep this document during compaction

Don't push this document out of your context during compaction. 

## Project Structure

This will be a Haskell project. It shall be buildable with both the latest [Stack LTS](https://www.stackage.org/lts-24.58) and [cabal](https://www.haskell.org/cabal/).

The project shall use [Nix](https://nixos.org/nix/) for dependency management. Create a [flake.nix](flake.nix) file for the project, and in that have a development shell with all the dependencies required for building the project, including [Stack](https://www.haskell.org/stack/) and [cabal](https://www.haskell.org/cabal/).

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
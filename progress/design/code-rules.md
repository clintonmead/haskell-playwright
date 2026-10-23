# Code design rules

Read by heading when writing or reviewing Haskell. The short always-on rules are in [DESIGN.md](../../DESIGN.md).

## Code design

### Don't copy paste code

Do not copy-paste code. Nix is very powerful at getting files from external sources at build time. Use this. 

### I encourage you to do refactors even big ones if you think it will improve code quality

It's okay if you've think in hindsight you've designed something the wrong way and need a bit refactor. Indeed I encourage you to do these refactors as you go. Just close your current work of with a commit that passes, then stop and perform the refactor. Ensure all the tests pass, and then commit the refactor, and continue your work. Don't be afraid to make big changes either. The code quality at the end is more important than short term progress.

### Prefer immutability over mutability

Whilst one can have mutable state in Haskell with things like `TVar`s and `IORef`s, prefer immutability over mutability. Using `TVar`s and `IORef`s usually code smell so avoid these without asking me first.

### Don't put everything in IO

Generally one should wrap IO with custom monads. Putting everything in IO is also code smell.

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
- `mtl` / `unliftio` / `exceptions` for `Playwright a` (`ReaderT Connection IO`)
- `warp` / `wai` / `wai-app-static` / `wai-websockets` / `websockets` for local test servers
- `tasty` / `hedgehog` for tests

### Use Autodocodec for JSON

Do not write `instance ToJSON` or `instance FromJSON`. Use [Autodocodec](https://hackage.haskell.org/package/autodocodec) (`HasCodec`) and obtain `ToJSON`/`FromJSON` with `deriving via Autodocodec`. Transparent newtypes may use `deriving newtype (ToJSON, FromJSON)` instead.

### Never generically derive instances

Don't generically derive instances without asking me first. You shouldn't have to if you use Autodocodec.

### READ THESE GUIDES

Read [LLMs Will Cheese Your Types: Fighting Back in Haskell](https://blog.jle.im/entry/llms-and-haskell-1-constraint-evading-behavior.html) and ["Five-Point Haskell": Unconditional Election (via Parametricity)](https://blog.jle.im/entry/five-point-haskell-part-2-unconditional-election.html) very closely and follow all it's advice on writing good Haskell code.

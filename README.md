# Haskell Learning Repository

This repository distills core and advanced Haskell concepts into small, runnable examples. Each file focuses on a theme—functions, types, monads, lazy evaluation, or real-world techniques—so you can explore the language one concept at a time.

## Repository Map

- `hello.hs` – friendly introduction plus REPL basics
- `functions.hs` – pure functions, higher-order utilities, recursion
- `typesystem.hs` – ADTs, GADTs, type families, type-level programming
- `monads.hs` – Maybe/Either/list monads, Writer/State patterns, transformers
- `lazyeval.hs` – infinite structures, strictness control, lazy IO
- `realworld.hs` – pragmatic patterns (pricing engine, todo CLI, concurrency)
- `exercises.hs` – practice problems with sample solutions
- `patterns.hs` – idiomatic Haskell design patterns and combinators
- `advanced-types.hs` – phantom/existential types, type families, API-level typing
- `concurrency.hs` – threads, STM, async, producer-consumer, resource pooling
- `functional-patterns.hs` – lenses, free/tagless DSLs, Yoneda, CPS, validation

## Highlights

### Pure Functional Foundations
- Currying, composition, and pipelines
- Recursion with guards and pattern matching
- Infinite lists and lazily generated sequences

### Type System Mastery
- Algebraic data types (sum/product), smart constructors
- Custom type classes, multi-parameter classes, associated types
- GADTs for typed interpreters and expression evaluators
- Type-level naturals, length-indexed vectors, and type families

### Monads & Effects
- Safe math with `Maybe` and explicit errors via `Either`
- Non-deterministic search using list monads
- Writer monad logging, State monad counters, and transformer stacks
- Kleisli composition for validation pipelines

### Lazy Evaluation & Performance
- Building and consuming infinite streams safely
- `seq`, strict folds, and bang patterns to remove thunks
- Lazy vs strict IO patterns and how to avoid space leaks

### Real-World Patterns
- Pure pricing logic with deterministic rules
- Functional ETL-style transaction summarization
- Disk-backed todo list CLI with JSON-free storage
- Concurrent metrics collector using `MVar`
- Resource-safe logging with `bracket`

### Advanced Type System & Abstractions
- Phantom types for domain modeling and unit safety
- Existential wrappers for heterogeneous collections
- Type families for configuration-driven APIs
- Higher-kinded type classes and type applications

### Concurrency & Functional Patterns
- `forkIO`, `Async`, STM, and structured concurrency recipes
- Producer/consumer queues, connection pools, rate limiting
- Lenses for immutable updates and nested data manipulation
- Free monads, tagless-final interpreters, Yoneda/Codensity optimizations
- Applicative validation pipelines and comonadic cellular automata

## Usage

Run any example directly with `runghc`:

```bash
runghc monads.hs
runghc lazyeval.hs
```

Or compile for faster execution:

```bash
ghc -O2 realworld.hs -o realworld
./realworld
```

## Learning Path

1. Start with `hello.hs` and `functions.hs`
2. Move to `typesystem.hs` for a deeper dive
3. Study `monads.hs` alongside `lazyeval.hs`
4. Apply concepts in `realworld.hs` and `patterns.hs`
5. Reinforce with the prompts inside `exercises.hs`

All samples are intentionally dependency-light and stick to `base` so they work out of the box with GHC.

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
- `advanced-fp.hs` – profunctors, optics, free monads, comonads, recursion schemes
- `web-api.hs` – Warp/Aeson REST service with middleware, uploads, health probes
- `category-theory.hs` – categorical intuition for functors, natural transformations, Yoneda
- `testing.hs` – manual tests, QuickCheck properties, generators, model-based testing
- `pure-functional.hs` – referential transparency, immutable pipelines, lazy streams
- `effect-systems.hs` – ReaderT/ExceptT stacks, capability type classes, pure interpreters
- `type-level.hs` – type-level programming, singletons, type families, dependent types
- `parsers.hs` – monadic parser combinators, JSON parsing, arithmetic expressions
- `transformers.hs` – advanced monad transformers, capability type classes, real-world stacks
- `quickcheck-properties.hs` – advanced property-based testing patterns and generators
- `performance-optimization.hs` – strictness control, memoization, vector usage, benchmarking
- `haskell-tooling.hs` – package management, testing frameworks, CI/CD, development tools

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

### Advanced FP Tooling
- Profunctor-based optics plus custom lenses/prisms
- Free monad DSLs with IO/state interpreters
- Contravariant structures, comonads, and recursion schemes
- Type-level vectors with length-safe concatenation

### Category Theory Foundations
- Category class implementation, functor laws, and natural transformations
- Yoneda lemma encoding plus comonads (NonEmpty, Store)
- Adjunction intuition via free/forgetful functors and Day convolution

### Testing & Property-Based Workflows
- Manual sanity checks plus QuickCheck suites and custom generators
- Data modeling for Arbitrary instances, round-trip properties, and shrinking
- Model-based tests, monoid/functor/monad law verification helpers

### Effect Systems & Transformers
- ReaderT/ExceptT/WriterT stack with capability type classes
- Production runner vs pure `StateT` interpreter for deterministic tests
- Real-world checkout flow with logging, metrics, retries, and exercises

### Type-Level Programming
- Singleton types for natural numbers and type-level vectors
- Type families for compile-time computation
- GADTs for type-safe indexing and heterogeneous collections
- Practical examples: type-safe matrix operations, unit systems, configuration

### Parser Combinators
- Building monadic parser combinators from scratch
- JSON parser implementation with recursive structure
- Arithmetic expression parsing with operator precedence
- CSV and configuration file parsing patterns

### Advanced Monad Transformers
- Layered transformer stacks for real applications
- Capability type classes vs direct transformer usage
- Free monad DSLs vs tagless-final approaches
- Error recovery with exponential backoff and testing with pure interpreters

### Applied Web APIs
- Warp server wiring with middleware stacking
- JSON encoding/decoding via Aeson with validation
- In-memory persistence using `MVar` for CRUD flows
- Health/readiness probes and multipart uploads

### Advanced Property-Based Testing
- Custom generators for complex data types
- Property combinators and conditional testing
- Shrinking demonstrations and failure minimization
- Stateful property testing with STM
- Round-trip properties and invariant preservation
- Performance testing with timeouts and memory bounds

### Performance Optimization Techniques
- Strictness control with bang patterns and strict data types
- Memory profiling and space leak detection
- Vector vs list performance comparisons
- Cache-friendly data structures (SoA vs AoS)
- Loop fusion and rewrite rules
- Inlining and specialization pragmas
- Unboxed types and primitive operations
- Worker pools and parallel execution patterns
- Stream fusion and lazy evaluation control

### Haskell Development Tooling
- Cabal and Stack package configuration
- Hspec test suites and Criterion benchmarks
- Git hooks for automated quality checks
- CI/CD pipelines with GitHub Actions
- Docker configurations for deployment
- Editor setup with Haskell Language Server
- Documentation generation with Haddock
- Dependency management and version bounds

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
5. Explore advanced topics: `type-level.hs`, `parsers.hs`, and `transformers.hs`
6. Reinforce with the prompts inside `exercises.hs`
7. Dive into advanced FP with `advanced-fp.hs` and `category-theory.hs`
8. Study practical applications with `web-api.hs` and `effect-systems.hs`
9. Master property-based testing with `quickcheck-properties.hs`
10. Optimize code with `performance-optimization.hs`
11. Set up development environment with `haskell-tooling.hs`

Most samples are intentionally dependency-light and stick to `base` so they work out of the box with GHC.
Some advanced modules (`quickcheck-properties.hs`, `performance-optimization.hs`) demonstrate real-world
scenarios and require external packages (QuickCheck, vector, bytestring, text). Use `COMPILE_CHECK.sh`
to see which files need dependencies.

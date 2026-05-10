# Haskell

Haskell labs and coursework — functional programming, monads, monad transformers, parallel processing.

## Labs

### lab2 — Typeclasses & Higher-Order Functions
- Custom `Transaction` ADT with pattern matching
- Higher-order `classifyTransactions` — partitions a list into 4 groups by two predicates
- `Maybe` chaining, folds, function composition

### lab3 — Stack project (custom data structures)

### lab4 — Expression Calculator (Cabal/Stack)
- Arithmetic expression parser and evaluator

### lab5 — Auth System with Monad Transformers
Token-based authentication using a monad transformer stack:
```
AuthStack = ExceptT AuthError (ReaderT Config IO)
```
- Config loaded from `config.json` (Aeson)
- `ReaderT` carries app config through the call stack
- `ExceptT` handles auth errors: `InvalidToken`, `NoAttemptsLeft`, `ForbiddenResource`
- Retry loop with attempt counter

### lab6 — Extended lab (tests)

## Coursework — PNM Image Processor

Parallel PNM image processing pipeline written from scratch:

- Custom PNM parser (P1–P6: ASCII and binary formats)
- Image effects: grayscale, invert, flip, brightness, contrast, sharpen, edge detection, threshold, glitch
- Parallel processing via `Control.Parallel.Strategies` (`parList`, `rdeepseq`)
- No image processing libraries — pure Haskell

## Build

```bash
# any stack project
cd lab4   # or lab5, lab6, Kur/part1, Kur/part2
stack build
stack exec <project-name>
stack test
```

## Requirements

- GHC 9.x
- Stack

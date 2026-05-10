# Haskell

Haskell labs and coursework — functional programming, typeclasses, monad transformers, BMP image processing.

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

---

## Coursework — BMP Image Processing System

**Variant 16.** A BMP image processing system written entirely in pure Haskell without external image libraries.

### BMP Parser

Low-level binary parser built on `Data.ByteString`:

```haskell
parseBMP :: B.ByteString -> Maybe BMP
```

Supports 1-bit (monochrome), 24-bit (RGB), and 32-bit (RGBA) BMP files. All multi-byte integers are read in Little Endian order:

```haskell
readWord16LE :: B.ByteString -> Int -> Word16
readWord32LE :: B.ByteString -> Int -> Word32
```

Core data types:

```haskell
data Color = Color { red, green, blue, alpha :: Word8 }

data BMPHeader = BMPHeader
  { fileSize     :: Word32
  , dataOffset   :: Word32 }

data DIBHeader = DIBHeader
  { width        :: Int
  , height       :: Int
  , bitsPerPixel :: Word16 }

data BMP = BMP
  { bmpHeader :: BMPHeader
  , dibHeader :: DIBHeader
  , pixels    :: [[Color]] }
```

### Chroma Key System

A full chroma key (green/blue screen) pipeline:

```haskell
data ChromaKey = ChromaKey
  { targetColor  :: Color
  , toleranceR   :: Int
  , toleranceG   :: Int
  , toleranceB   :: Int
  , softEdge     :: Bool }
```

Pixel matching uses Euclidean distance in RGB space:

```haskell
colorDistance :: Color -> Color -> Double
colorDistance c1 c2 = sqrt $ fromIntegral $
  (r1-r2)^2 + (g1-g2)^2 + (b1-b2)^2
```

Built-in presets: `greenScreenKey`, `blueScreenKey`.

### Image Effects

10+ effects applied per-pixel or via convolution kernels:

| Effect | Implementation |
|--------|----------------|
| `applyGrayscale` | Weighted luminance: `0.299R + 0.587G + 0.114B` |
| `applyBrightness` | Add offset, clamp to `[0, 255]` |
| `applyInvert` | `255 - channel` per channel |
| `applyFlipH` | Reverse each row |
| `applyFlipV` | Reverse row order |
| `applyThreshold` | Binary black/white by luminance threshold |
| `applyGlitch` | Random row shifts using `System.Random.StdGen` |
| `applySharpen` | 3×3 kernel `[[0,-1,0],[-1,5,-1],[0,-1,0]]` |
| `applyGaussianBlur` | 3×3 Gaussian kernel |
| `applyCustomKernel` | User-supplied convolution matrix |

### Multi-Layer Composition

Images are composed from multiple layers with per-layer settings:

```haskell
renderComposition :: Composition -> BMP
```

Each layer supports an independent chroma key and a blend mode:

| Blend Mode | Formula |
|------------|---------|
| Normal     | top pixel replaces bottom |
| Multiply   | `(A * B) / 255` |
| Screen     | `255 - ((255-A) * (255-B)) / 255` |
| Overlay    | combination of Multiply and Screen by luminance |
| Additive   | `clamp(A + B, 0, 255)` |

### Console UI

Interactive loop with full undo/redo support:

```haskell
data AppState = AppState
  { currentImage       :: Maybe BMP
  , currentComposition :: Composition
  , undoStack          :: [BMP]
  , redoStack          :: [BMP] }
```

`mainLoop` is tail-recursive — every destructive operation pushes the previous state onto `undoStack` and clears `redoStack`. Undo pops from `undoStack`; redo pops from `redoStack`.

---

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

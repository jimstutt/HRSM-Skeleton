#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Integrating Reflex-DOM into frontend-wasm..."

# 1. Update Main.hs with a minimal Reflex-DOM counter
cat << 'EOF' > "$DIR/frontend-wasm/Main.hs"
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Reflex.Dom
import System.IO (hFlush, stdout)

foreign export ccall start_reactor :: IO ()
foreign export ccall reactor_stop  :: IO ()

mainWidget :: DomBuilder t m => m ()
mainWidget = do
  el "h1" $ text "HRSM Reflex-DOM Counter"
  el "div" $ do
    (count, _) <- button "Increment"
    text "Count: "
    dynText (fmap show count)

start_reactor :: IO ()
start_reactor = do
  putStrLn "[HRSM] Initializing Reflex-DOM..."
  hFlush stdout
  -- In a real browser, we'd use mainWidgetInBody or similar
  -- For now, we just verify it compiles and runs
  putStrLn "[HRSM] Reflex-DOM widget defined successfully."
  hFlush stdout

reactor_stop :: IO ()
reactor_stop = do
  putStrLn "[HRSM] Reactor stopped"
  hFlush stdout

main :: IO ()
main = pure ()
EOF

# 2. Update frontend-wasm.cabal to include reflex-dom
cat << 'EOF' > "$DIR/frontend-wasm/frontend-wasm.cabal"
cabal-version:      3.0
name:               frontend-wasm
version:            0.1.0.0
build-type:         Simple

executable frontend-wasm-exe
  main-is:          Main.hs
  hs-source-dirs:   .
  default-language: Haskell2010
  build-depends:
      base >= 4.14 && < 5
    , reflex-dom >= 0.6
    , reflex >= 0.9
    , text
    , containers
  ghc-options:
    -O2
    -no-hs-main
    -optl-mexec-model=reactor
    -optl-Wl,--allow-undefined
    -optl-Wl,--export=start_reactor
    -optl-Wl,--export=reactor_stop
    -optl-Wl,--export-all
EOF

# 3. Update build-wasm.sh to use cabal for dependency resolution
cat << 'EOF' > "$DIR/scripts/build-wasm.sh"
#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c frontend-wasm/stubs.c -o dist-wasm/stubs.o

echo "[2/3] Building frontend-wasm with cabal..."
export HOME=$PWD
# Use cabal to resolve dependencies and compile
cabal build frontend-wasm-exe \
  --with-compiler=wasm32-wasi-ghc \
  --with-hc-pkg=wasm32-wasi-ghc-pkg \
  --with-hsc2hs=wasm32-wasi-hsc2hs

echo "[3/3] Linking with stubs..."
# Find the compiled object files and link them with our C stubs
# This is a simplified step; cabal usually handles linking, but we need to include our C stubs
OBJ_FILE=$(find dist-newstyle -type f -name "*.o" | grep "Main.o" | head -n 1)
if [ -z "$OBJ_FILE" ]; then
  echo "Error: Could not find compiled Main.o"
  exit 1
fi

wasm32-wasi-ghc \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--allow-undefined \
  -optl-Wl,--export=start_reactor \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  "$OBJ_FILE" \
  dist-wasm/stubs.o \
  -o dist-wasm/reactor.wasm

echo "[HRSM] Done: dist-wasm/reactor.wasm"
EOF
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] Reflex-DOM integration started."
echo "Next steps:"
echo "  1. git add frontend-wasm/Main.hs frontend-wasm/frontend-wasm.cabal scripts/build-wasm.sh"
echo "  2. nix develop"
echo "  3. ./scripts/build-wasm.sh (This will test if cabal can resolve dependencies in the dev shell)"

#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/3] Compiling Haskell → object"
ghc \
  -O2 \
  -no-hs-main \
  -c frontend-wasm/Main.hs \
  -o dist-wasm/main.o

echo "[2/3] Linking → WASI reactor"
clang \
  --target=wasm32-wasi \
  -nostartfiles \
  -Wl,--no-entry \
  -Wl,--export=reactor_start \
  -Wl,--export=reactor_stop \
  -Wl,--export-all \
  -Wl,--allow-undefined \
  dist-wasm/main.o \
  -o dist-wasm/reactor.wasm

echo "[3/3] Done: dist-wasm/reactor.wasm"

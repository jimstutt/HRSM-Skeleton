#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c frontend-wasm/stubs.c -o dist-wasm/stubs.o

echo "[2/3] Compiling and Linking Haskell with stubs → WASI reactor"
wasm32-wasi-ghc \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--allow-undefined \
  -optl-Wl,--export=start_reactor \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  frontend-wasm/Main.hs \
  dist-wasm/stubs.o \
  -o dist-wasm/reactor.wasm

echo "[3/3] Done: dist-wasm/reactor.wasm"

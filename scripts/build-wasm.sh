#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[build-wasm] Using GHC:"
which ghc

ghc \
  -no-hs-main \
  -optl-mexec-model=reactor \
  frontend-wasm/Main.hs \
  -o dist-wasm/reactor.wasm

echo "[build-wasm] Built dist-wasm/reactor.wasm"

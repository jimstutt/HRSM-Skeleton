#!/usr/bin/env bash
set -e

echo "🔍 Searching for built wasm..."
wasm_file=$(find dist-wasm -type f -name "*.wasm" | head -n 1)

if [ -z "$wasm_file" ]; then
  echo "❌ No .wasm file found. Did you run 'cabal build'?"
  exit 1
fi

echo "▶ Running via wasmtime: $wasm_file"
wasmtime "$wasm_file"

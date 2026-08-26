#!/usr/bin/env bash
W=$(find /nix/store -name "ghc-iserv.wasm" -path "*/lib/bin/*" 2>/dev/null | head -n 1)
[ -z "$W" ] && { echo "Error: ghc-iserv.wasm not found" >&2; exit 1; }
exec wasmtime run "$W" "$@"

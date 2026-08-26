import os
D = "/home/jimstutt/Dev/HRSM-Skeleton"

# 1. Fix cabal.project to remove the custom -pgmi flag
cabal_proj = """packages:
  .

package *
  ghc-options: -fexternal-interpreter
"""
with open(os.path.join(D, "frontend-wasm", "cabal.project"), "w") as f:
    f.write(cabal_proj)

# 2. Fix build-wasm.sh to remove the custom -pgmi flag
build_sh = """#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with wasm32-wasi-cabal..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"
unset GHC_PACKAGE_PATH

# Temporarily rename root cabal.project to prevent workspace contamination
[ -f "$DIR/cabal.project" ] && mv "$DIR/cabal.project" "$DIR/cabal.project.bak"
rm -rf "$DIR/dist-newstyle/cache/plan.json" "$DIR/frontend-wasm/dist-newstyle"

wasm32-wasi-cabal update

cd "$DIR/frontend-wasm"
# Rely on wasm32-wasi-cabal's built-in external interpreter support for TH
wasm32-wasi-cabal build frontend-wasm-exe --ghc-options="-fexternal-interpreter"

cd "$DIR"
[ -f "$DIR/cabal.project.bak" ] && mv "$DIR/cabal.project.bak" "$DIR/cabal.project"

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$OBJ_FILE" ] && { echo "Error: Main.o not found"; exit 1; }

wasm32-wasi-ghc -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$OBJ_FILE" "$DIR/dist-wasm/stubs.o" -o "$DIR/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
"""
with open(os.path.join(D, "scripts", "build-wasm.sh"), "w") as f:
    f.write(build_sh)
os.chmod(os.path.join(D, "scripts", "build-wasm.sh"), 0o755)

print("[HRSM] Removed custom -pgmi flag. Relying on wasm32-wasi-cabal's built-in TH support.")

import os
D = "/home/jimstutt/Dev/HRSM-Skeleton"

# 1. Create cabal.project.local INSIDE frontend-wasm to avoid root conflicts
cabal_local = """allow-newer:
  all:Cabal, all:Cabal-syntax, all:array, all:base, all:binary,
  all:bytestring, all:containers, all:deepseq, all:directory,
  all:exceptions, all:filepath, all:ghc, all:ghc-bignum, all:ghc-boot,
  all:ghc-boot-th, all:ghc-compact, all:ghc-experimental, all:ghc-heap,
  all:ghc-internal, all:ghc-platform, all:ghc-prim, all:ghc-toolchain,
  all:ghci, all:haskeline, all:hpc, all:integer-gmp, all:mtl,
  all:os-string, all:parsec, all:pretty, all:process, all:rts,
  all:semaphore-compat, all:stm, all:system-cxx-std-lib,
  all:template-haskell, all:text, all:time, all:transformers,
  all:unix, all:xhtml

constraints:
  Cabal installed, Cabal-syntax installed, array installed, base installed,
  binary installed, bytestring installed, containers installed, deepseq installed,
  directory installed, exceptions installed, filepath installed, ghc installed,
  ghc-bignum installed, ghc-boot installed, ghc-boot-th installed,
  ghc-compact installed, ghc-experimental installed, ghc-heap installed,
  ghc-internal installed, ghc-platform installed, ghc-prim installed,
  ghc-toolchain installed, ghci installed, haskeline installed, hpc installed,
  integer-gmp installed, mtl installed, os-string installed, parsec installed,
  pretty installed, process installed, rts installed, semaphore-compat installed,
  stm installed, system-cxx-std-lib installed, template-haskell installed,
  text installed, time installed, transformers installed, unix installed, xhtml installed
"""
with open(os.path.join(D, "frontend-wasm", "cabal.project.local"), "w") as f:
    f.write(cabal_local)

# Remove the one in the root if it exists
root_local = os.path.join(D, "cabal.project.local")
if os.path.exists(root_local):
    os.remove(root_local)

# 2. Restore root cabal.project if it was backed up
if os.path.exists(os.path.join(D, "cabal.project.bak")):
    os.rename(os.path.join(D, "cabal.project.bak"), os.path.join(D, "cabal.project"))

# 3. Update build-wasm.sh to NOT move cabal.project, just build from frontend-wasm
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

# Clear stale cache
rm -rf "$DIR/dist-newstyle/cache/plan.json" "$DIR/frontend-wasm/dist-newstyle"

wasm32-wasi-cabal update

cd "$DIR/frontend-wasm"
# Build directly in the frontend-wasm directory where cabal.project.local is located
wasm32-wasi-cabal build frontend-wasm-exe

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$OBJ_FILE" ] && { echo "Error: Main.o not found"; exit 1; }

wasm32-wasi-ghc -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$OBJ_FILE" "$DIR/dist-wasm/stubs.o" -o "$DIR/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
"""
with open(os.path.join(D, "scripts", "build-wasm.sh"), "w") as f:
    f.write(build_sh)
os.chmod(os.path.join(D, "scripts", "build-wasm.sh"), 0o755)

print("[HRSM] Moved cabal.project.local to frontend-wasm/ and restored root cabal.project.")

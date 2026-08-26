import os
D = "/home/jimstutt/Dev/HRSM-Skeleton"

# 1. Create cabal.project.local with the exact constraints from ghc-wasm-meta
# This prevents Cabal from trying to rebuild boot libraries from Hackage
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
with open(os.path.join(D, "cabal.project.local"), "w") as f:
    f.write(cabal_local)

# 2. Revert frontend-wasm.cabal back to Reflex-DOM (since TH will now work)
cabal_wasm = """cabal-version: 3.0
name: frontend-wasm
version: 0.1.0.0
build-type: Simple

executable frontend-wasm-exe
  main-is: Main.hs
  hs-source-dirs: .
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
"""
with open(os.path.join(D, "frontend-wasm", "frontend-wasm.cabal"), "w") as f:
    f.write(cabal_wasm)

# 3. Update build-wasm.sh to rely on wasm32-wasi-cabal's native TH support
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

# Temporarily hide root cabal.project to isolate frontend build
[ -f "$DIR/cabal.project" ] && mv "$DIR/cabal.project" "$DIR/cabal.project.bak"
rm -rf "$DIR/dist-newstyle/cache/plan.json" "$DIR/frontend-wasm/dist-newstyle"

wasm32-wasi-cabal update

cd "$DIR/frontend-wasm"
# wasm32-wasi-cabal automatically handles TH external interpreter when constraints are met
wasm32-wasi-cabal build frontend-wasm-exe

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

print("[HRSM] Applied ghc-wasm-meta cabal.project.local constraints. Reflex-DOM TH should now work.")

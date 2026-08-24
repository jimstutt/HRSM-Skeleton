#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"
rm -rf "$DIR/.cabal" "$HOME/.config/cabal"
mkdir -p "$DIR/frontend-wasm"

printf 'packages:\n  .\n' > "$DIR/frontend-wasm/cabal.project"

cat > "$DIR/frontend-wasm/frontend-wasm.cabal" << 'CABAL'
cabal-version: 3.0
name: frontend-wasm
version: 0.1.0.0
build-type: Simple
executable frontend-wasm-exe
  main-is: Main.hs
  hs-source-dirs: .
  default-language: Haskell2010
  build-depends: base >= 4.14 && < 5, reflex-dom >= 0.6, reflex >= 0.9, text, containers
  ghc-options: -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all
CABAL

cat > "$DIR/scripts/build-wasm.sh" << 'BUILD'
#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"
echo "[1/3] Compiling C stubs..."
clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"
echo "[2/3] Building frontend-wasm with cabal (GHC 9.8)..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"
cabal update
cabal build frontend-wasm:frontend-wasm-exe --project-file="$DIR/frontend-wasm/cabal.project" --with-compiler=wasm32-wasi-ghc-9.8 --with-ghc-pkg=wasm32-wasi-ghc-pkg-9.8 --with-hsc2hs=wasm32-wasi-hsc2hs-9.8
echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
if [ -z "$OBJ_FILE" ]; then echo "Error: Could not find compiled Main.o"; exit 1; fi
wasm32-wasi-ghc-9.8 -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$OBJ_FILE" "$DIR/dist-wasm/stubs.o" -o "$DIR/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
BUILD
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] Files updated. Running build..."
"$DIR/scripts/build-wasm.sh"

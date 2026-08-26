#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Relocating Wasm Cabal directory to .cabal-wasm..."

cat > "$DIR/scripts/build-wasm.sh" << 'EOF'
#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

# Use isolated Wasm-specific Cabal directory (not project root)
export CABAL_DIR="$DIR/.cabal-wasm"
export CABAL_CONFIG="$DIR/.cabal-wasm/config"
unset HOME  # Prevent Cabal from falling back to $HOME/.cabal

mkdir -p "$CABAL_DIR"

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with wasm32-wasi-cabal..."
rm -rf "$CABAL_DIR/store" "$DIR/frontend-wasm/dist-newstyle"

wasm32-wasi-cabal update

cd "$DIR/frontend-wasm"
wasm32-wasi-cabal build frontend-wasm-exe

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$OBJ_FILE" ] && { echo "Error: Main.o not found"; exit 1; }

wasm32-wasi-ghc -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$OBJ_FILE" "$DIR/dist-wasm/stubs.o" -o "$DIR/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
EOF

chmod +x "$DIR/scripts/build-wasm.sh"

# Add .cabal-wasm to gitignore
if ! grep -q ".cabal-wasm" "$DIR/.gitignore" 2>/dev/null; then
  echo ".cabal-wasm/" >> "$DIR/.gitignore"
  echo "[HRSM] Added .cabal-wasm/ to .gitignore"
fi

echo "[HRSM] Build script updated. Wasm Cabal state now isolated in .cabal-wasm/"

#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Setting up Wasm external interpreter for Template Haskell..."

# 1. Add wasmtime to flake.nix devShell (if not already present)
if ! grep -q "pkgs.wasmtime" "$DIR/flake.nix"; then
  sed -i '/pkgs.pkg-config/a \            pkgs.wasmtime' "$DIR/flake.nix"
fi

# 2. Create the wrapper script to run ghc-iserv.wasm via wasmtime
cat > "$DIR/scripts/wasm-iserv.sh" << 'ISERV_END'
#!/usr/bin/env bash
GHC_BIN_DIR=$(dirname $(which wasm32-wasi-ghc))
GHC_ROOT=$(dirname "$GHC_BIN_DIR")
exec wasmtime run "$GHC_ROOT/lib/bin/ghc-iserv.wasm" "$@"
ISERV_END
chmod +x "$DIR/scripts/wasm-iserv.sh"

# 3. Update cabal.project to use the wrapper via -pgmi
cat > "$DIR/frontend-wasm/cabal.project" << 'PROJECT_END'
packages:
  .

package *
  ghc-options: -fexternal-interpreter -pgmi ../scripts/wasm-iserv.sh
PROJECT_END

echo "[HRSM] Wrapper created and cabal.project updated."
echo "Please run: exit, nix develop, ./scripts/build-wasm.sh"

#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Updating wasm-iserv.sh to dynamically find ghc-iserv.wasm..."

cat > "$DIR/scripts/wasm-iserv.sh" << 'ISERV_END'
#!/usr/bin/env bash
# Dynamically find ghc-iserv.wasm in the Nix store
GHC_ISERV_WASM=$(find /nix/store -name "ghc-iserv.wasm" -path "*/lib/bin/*" 2>/dev/null | head -n 1)
if [ -z "$GHC_ISERV_WASM" ]; then
  echo "Error: Could not find ghc-iserv.wasm in /nix/store" >&2
  exit 1
fi
exec wasmtime run "$GHC_ISERV_WASM" "$@"
ISERV_END
chmod +x "$DIR/scripts/wasm-iserv.sh"

cat > "$DIR/frontend-wasm/cabal.project" << 'PROJECT_END'
packages:
  .

package *
  ghc-options: -fexternal-interpreter -pgmi ../scripts/wasm-iserv.sh
PROJECT_END

echo "[HRSM] Setup verified. Please run: nix develop, then ./scripts/build-wasm.sh"

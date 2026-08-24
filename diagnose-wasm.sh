#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Diagnosing Wasm overlay configuration..."

# Check if the overlay file exists
if [ -f "$DIR/nix/wasm-overlay" ]; then
    echo "✓ nix/wasm-overlay exists"
    echo "Contents:"
    cat "$DIR/nix/wasm-overlay"
else
    echo "✗ nix/wasm-overlay does not exist"
fi

echo ""
echo "[HRSM] Checking what Haskell package sets are available after overlay..."

# Create a temporary Nix expression to inspect the overlay
cat << 'EOF' > /tmp/inspect-overlay.nix
let
  pkgs = import <nixpkgs> { 
    overlays = [ (import ./nix/wasm-overlay) ];
  };
in {
  haskellPackageNames = builtins.attrNames pkgs.haskell.packages;
  hasGhcWasm = builtins.hasAttr "ghcWasm" pkgs.haskell.packages;
  hasGhcWasi = builtins.hasAttr "ghc-wasm32-wasi" pkgs.haskell.packages;
}
EOF

cd "$DIR"
nix-instantiate --eval --strict /tmp/inspect-overlay.nix 2>&1 || echo "Overlay evaluation failed"

echo ""
echo "[HRSM] Checking available build scripts..."
ls -la "$DIR/scripts/" 2>/dev/null || echo "No scripts directory"

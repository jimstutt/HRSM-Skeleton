#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Removing broken frontend-wasm package from flake.nix..."

cat << 'EOF' > "$DIR/flake.nix"
{
  description = "HRSM-Skeleton: Haskell Wasm Reflex Servant App";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system; 
          config = { allowBroken = true; };
        };
        
        # Standard GHC for backend and common
        haskellPkgs = pkgs.haskellPackages;

        # 1. Build the local 'common' package first
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        
        # 2. Build 'backend', explicitly passing the local 'common' package
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend {
          common = commonPkg;
        };

        # NOTE: frontend-wasm is NOT built via nix build
        # Wasm compilation is handled by scripts/build-wasm.sh
        # which uses the custom wasm32-wasi-sdk from pkgs/

        # Emacs 30 package set
        emacsPkgs = pkgs.emacsPackagesFor pkgs.emacs30;

      in
      {
        packages = {
          inherit commonPkg backendPkg;
          common = commonPkg;
          backend = backendPkg;
          default = backendPkg;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server
            pkgs.mariadb
            pkgs.pkg-config
            
            # Project-specific Emacs with gptel injected
            (emacsPkgs.emacsWithPackages (epkgs: [
              epkgs.gptel
            ]))
          ];
          
          shellHook = ''
            echo "[HRSM] Development shell loaded."
            echo " - Backend: nix build .#backend"
            echo " - Frontend Wasm: ./scripts/build-wasm.sh"
            echo " - Emacs with gptel is available in this shell."
          '';
        };
      }
    );
}
EOF

echo "[HRSM] flake.nix updated successfully."
echo ""
echo "Correct workflow:"
echo "  - Backend: nix build .#backend"
echo "  - Frontend Wasm: ./scripts/build-wasm.sh (uses custom wasm32-wasi-sdk)"
echo ""
echo "The frontend-wasm/ directory contains Haskell source code,"
echo "but it is compiled to Wasm via scripts/build-wasm.sh, not nix build."

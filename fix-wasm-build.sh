#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Restoring Wasm overlay and fixing frontend build configuration..."

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
        # Apply the project's custom Wasm overlay to get the correct GHC Wasm package set
        pkgs = import nixpkgs { 
          inherit system; 
          overlays = [ (import ./nix/wasm-overlay) ];
          config = { allowBroken = true; };
        };
        
        # Standard GHC for backend and common
        haskellPkgs = pkgs.haskellPackages;
        
        # Wasm GHC for frontend (provided by the wasm-overlay)
        # Fallback to ghc-wasm32-wasi if the overlay names it differently
        haskellWasmPkgs = pkgs.haskell.packages.ghcWasm or pkgs.haskell.packages.ghc-wasm32-wasi or pkgs.haskellPackages;

        # 1. Build the local 'common' package first
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        
        # 2. Build 'backend', explicitly passing the local 'common' package
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend {
          common = commonPkg;
        };

        # 3. Build 'frontend-wasm' using the Wasm package set
        frontendWasmPkg = haskellWasmPkgs.callCabal2nix "frontend-wasm" ./frontend-wasm {
          common = commonPkg;
        };

        # Emacs 30 package set
        emacsPkgs = pkgs.emacsPackagesFor pkgs.emacs30;

      in
      {
        packages = {
          inherit commonPkg backendPkg frontendWasmPkg;
          common = commonPkg;
          backend = backendPkg;
          frontend-wasm = frontendWasmPkg;
          default = backendPkg;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server
            pkgs.mariadb
            pkgs.pkg-config # Added to prevent pkg-config missing errors for native tools
            
            # Project-specific Emacs with gptel injected
            (emacsPkgs.emacsWithPackages (epkgs: [
              epkgs.gptel
            ]))
          ];
          
          shellHook = ''
            echo "[HRSM] Development shell loaded."
            echo " - Backend: nix build .#backend"
            echo " - Frontend: nix build .#frontend-wasm"
            echo " - Emacs with gptel is available in this shell."
          '';
        };
      }
    );
}
EOF

echo "[HRSM] flake.nix updated with Wasm overlay."
echo "Next step: Run 'nix build .#frontend-wasm' again."

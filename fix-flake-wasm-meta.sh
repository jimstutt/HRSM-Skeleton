#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Integrating ghc-wasm-meta into flake.nix for proper Reflex-DOM Wasm compilation..."

cat << 'EOF' > "$DIR/flake.nix"
{
  description = "HRSM-Skeleton: Haskell Wasm Reflex Servant App";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    # Add ghc-wasm-meta for proper Haskell -> WebAssembly compilation
    ghc-wasm-meta.url = "github:haskell-wasm/ghc-wasm-meta";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system; 
          config = { allowBroken = true; };
        };
        
        # Standard GHC for backend and common
        haskellPkgs = pkgs.haskellPackages;
        
        # Wasm GHC package set from ghc-wasm-meta
        wasmHaskellPkgs = ghc-wasm-meta.packages.${system}.wasm32-wasi-ghc;

        # 1. Build the local 'common' package first
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        
        # 2. Build 'backend', explicitly passing the local 'common' package
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend {
          common = commonPkg;
        };

        # 3. Build 'frontend-wasm' using the Wasm Haskell package set
        frontendWasmPkg = wasmHaskellPkgs.callCabal2nix "frontend-wasm" ./frontend-wasm {
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
            pkgs.pkg-config
            # Make the Wasm GHC compiler available in the dev shell
            wasmHaskellPkgs.ghc 
            
            # Project-specific Emacs with gptel injected
            (emacsPkgs.emacsWithPackages (epkgs: [
              epkgs.gptel
            ]))
          ];
          
          shellHook = ''
            echo "[HRSM] Development shell loaded."
            echo " - Backend: nix build .#backend"
            echo " - Frontend Wasm: nix build .#frontend-wasm"
            echo " - Emacs with gptel is available in this shell."
          '';
        };
      }
    );
}
EOF

echo "[HRSM] flake.nix updated with ghc-wasm-meta."
echo "Next step: Run 'nix build .#frontend-wasm'"

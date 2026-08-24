#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Updating flake.nix to build frontend-wasm via scripts/build-wasm.sh..."

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

        # 3. Build 'frontend-wasm' using the project's build-wasm.sh script
        frontendWasmPkg = pkgs.stdenv.mkDerivation {
          pname = "frontend-wasm";
          version = "0.1.0.0";
          src = ./.;

          nativeBuildInputs = [
            pkgs.ghc
            pkgs.clang
          ];

          buildPhase = ''
            echo "[HRSM] Building Wasm frontend via scripts/build-wasm.sh"
            bash ./scripts/build-wasm.sh
          '';

          installPhase = ''
            mkdir -p $out
            cp dist-wasm/reactor.wasm $out/
            echo "[HRSM] Wasm frontend built successfully: $out/reactor.wasm"
          '';
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
            pkgs.ghc
            pkgs.clang
            
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

echo "[HRSM] flake.nix updated successfully."
echo "Next step: Run 'nix build .#frontend-wasm'"
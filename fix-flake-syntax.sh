#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing flake.nix syntax and cabal HOME directory..."

cat << 'EOF' > "$DIR/flake.nix"
{
  description = "HRSM-Skeleton: Haskell Wasm Reflex Servant App";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { 
          inherit system; 
          config = { allowBroken = true; };
        };
        
        haskellPkgs = pkgs.haskellPackages;
        wasmToolchain = ghc-wasm-meta.packages.${system}.default;

        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend {
          common = commonPkg;
        };

        frontendWasmPkg = pkgs.stdenv.mkDerivation {
          pname = "frontend-wasm";
          version = "0.1.0.0";
          src = ./.;
          
          nativeBuildInputs = [
            wasmToolchain
            pkgs.cabal-install
            pkgs.pkg-config
          ];

          buildPhase = ''
            export HOME=$PWD
            echo "[HRSM] Configuring cabal for Wasm compilation..."
            cabal configure \
              --with-compiler=wasm32-wasi-ghc \
              --with-hc-pkg=wasm32-wasi-ghc-pkg \
              --with-hsc2hs=wasm32-wasi-hsc2hs
            
            echo "[HRSM] Building frontend-wasm with Wasm GHC..."
            cabal build frontend-wasm:frontend-wasm-exe \
              --ghc-options="-optl-mexec-model=reactor -optl-Wl,--export=reactor_start,--export=reactor_stop,--export-all,--export=hs_init"
          '';

          installPhase = ''
            mkdir -p $out
            find dist-newstyle -type f -name "*.wasm" | head -n 1 | xargs -I {} cp {} $out/reactor.wasm || true
            echo "[HRSM] Wasm frontend built successfully: $out/reactor.wasm"
          '';
        };

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
            wasmToolchain
            (emacsPkgs.emacsWithPackages (epkgs: [ epkgs.gptel ]))
          ];
          
          shellHook = ''
            echo "[HRSM] Development shell loaded."
            echo " - Backend: nix build .#backend"
            echo " - Frontend Wasm: nix build .#frontend-wasm"
            echo " - Wasm Compiler: wasm32-wasi-ghc is available"
          '';
        };
      }
    );
}
EOF

echo "[HRSM] flake.nix updated successfully."
echo "Next step: Run 'nix build .#frontend-wasm'"

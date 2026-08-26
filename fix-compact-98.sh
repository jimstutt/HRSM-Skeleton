#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"
echo "[HRSM] Forcing explicit GHC 9.8 Wasm toolchain..."

cat > "$DIR/flake.nix" << 'EOF'
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
        pkgs = import nixpkgs { inherit system; config = { allowBroken = true; }; };
        haskellPkgs = pkgs.haskellPackages;
        wasmGhc = ghc-wasm-meta.packages.${system}.wasm32-wasi-ghc-9_8;
        wasmCabal = ghc-wasm-meta.packages.${system}.wasm32-wasi-cabal-9_8;
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend { common = commonPkg; };
        frontendWasmPkg = pkgs.stdenv.mkDerivation {
          pname = "frontend-wasm"; version = "0.1.0.0"; src = ./.;
          nativeBuildInputs = [ wasmGhc wasmCabal pkgs.pkg-config ];
          buildPhase = ''
            export HOME=$PWD; export CABAL_DIR=$PWD/.cabal
            wasm32-wasi-cabal-9_8 configure --project-dir=frontend-wasm --disable-documentation --disable-tests --disable-benchmarks
            wasm32-wasi-cabal-9_8 build frontend-wasm-exe --project-dir=frontend-wasm
          '';
          installPhase = ''
            mkdir -p $out
            find dist-newstyle -type f -name "*.wasm" | head -n 1 | xargs -I {} cp {} $out/reactor.wasm || true
          '';
        };
      in {
        packages = { inherit commonPkg backendPkg frontendWasmPkg; common = commonPkg; backend = backendPkg; frontend-wasm = frontendWasmPkg; default = backendPkg; };
        devShells.default = pkgs.mkShell {
          buildInputs = [ haskellPkgs.cabal-install haskellPkgs.haskell-language-server pkgs.mariadb pkgs.pkg-config pkgs.wasmtime wasmGhc wasmCabal ];
          shellHook = '' echo "[HRSM] Dev shell loaded. Wasm Compiler: wasm32-wasi-ghc-9_8"; '';
        };
      }
    );
}
EOF

cat > "$DIR/scripts/build-wasm.sh

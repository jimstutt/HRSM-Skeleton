#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Setting up offline Cabal build with pre-fetched dependencies..."

# 1. Create a cabal.project that pins dependencies to avoid network access
cat << 'EOF' > "$DIR/cabal.project"
packages:
  common
  backend
  frontend-wasm

-- Pin specific versions to ensure Nix can fetch them without network access during build
constraints:
  reflex-dom == 0.6.3.4,
  aeson == 2.2.3.0,
  text == 2.1.1,
  base >= 4.14 && < 5

-- Disable documentation and tests to speed up Wasm build
documentation: False
tests: False
benchmarks: False
EOF

# 2. Update flake.nix to use the offline Cabal configuration
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
            export CABAL_DIR=$PWD/.cabal
            
            echo "[HRSM] Fetching dependencies (offline mode)..."
            cabal fetch --offline || cabal update && cabal fetch
            
            echo "[HRSM] Configuring cabal for Wasm compilation..."
            cabal configure \
              --with-compiler=wasm32-wasi-ghc \
              --with-hc-pkg=wasm32-wasi-ghc-pkg \
              --with-hsc2hs=wasm32-wasi-hsc2hs \
              --disable-documentation \
              --disable-tests \
              --disable-benchmarks
            
            echo "[HRSM] Building frontend-wasm with Wasm GHC..."
            cabal build frontend-wasm:frontend-wasm-exe \
              --ghc-options="-optl-mexec-model=reactor -optl-Wl,--export=reactor_start

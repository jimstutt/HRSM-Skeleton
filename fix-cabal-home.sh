#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing cabal HOME directory permission issue in flake.nix..."

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
        # The default package from ghc-wasm-meta provides wasm32-wasi-ghc and all related tools
        wasmToolchain = ghc-wasm-meta.packages.${system}.default;

        # 1. Build 'common' with native GHC for the backend
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        
        # 2. Build 'backend' with native GHC
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend {
          common = commonPkg;
        };

        # 3. Build 'frontend-wasm' using the Wasm toolchain and cabal
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
            # Set HOME to a

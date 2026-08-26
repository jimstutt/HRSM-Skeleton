import os
DIR = "/home/jimstutt/Dev/HRSM-Skeleton"

flake = """{
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
          shellHook = "echo '[HRSM] Dev shell loaded. Wasm Compiler: wasm32-wasi-ghc-9_8'";
        };
      }
    );
}"""

build = """#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"
echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"
echo "[2/3] Building frontend-wasm with wasm32-wasi-cabal-9_8..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"
unset GHC_PACKAGE_PATH
wasm32-wasi-cabal-9_8 update
wasm32-wasi-cabal-9_8 build frontend-wasm-exe --project-dir="$DIR/frontend-wasm" --ghc-options="-fexternal-interpreter -pgmi $DIR/scripts/wasm-iserv.sh"
echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$OBJ_FILE" ] && { echo "Error: Could not find compiled Main.o"; exit 1; }
wasm32-wasi-ghc-9_8 -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$OBJ_FILE" "$DIR/dist-wasm/stubs.o" -o "$DIR/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"

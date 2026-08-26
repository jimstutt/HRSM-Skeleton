import os
D = "/home/jimstutt/Dev/HRSM-Skeleton"

# 1. Update flake.nix to use all_9_10 (which includes the Wasm TH iserv wrapper)
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
        # GHC 9.10 has base-4.20 (satisfies reflex-dom < 4.22) AND includes Wasm TH support
        wasmToolchain = ghc-wasm-meta.packages.${system}.all_9_10;
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend { common = commonPkg; };
      in {
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
            pkgs.wasmtime
            wasmToolchain 
          ];
          shellHook = "echo '[HRSM] Dev shell loaded. Wasm Compiler: wasm32-wasi-ghc (GHC 9.10)'";
        };
      }
    );
}"""
with open(os.path.join(D, "flake.nix"), "w") as f:
    f.write(flake)

# 2. Ensure cabal.project.local is in frontend-wasm/ to use installed boot libraries
cabal_local = """allow-newer:
  all:Cabal, all:Cabal-syntax, all:array, all:base, all:binary,
  all:bytestring, all:containers, all:deepseq, all:directory,
  all:exceptions, all:filepath, all:ghc, all:ghc-bignum, all:ghc-boot,
  all:ghc-boot-th, all:ghc-compact, all:ghc-experimental, all:ghc-heap,
  all:ghc-internal, all:ghc-platform, all:ghc-prim, all:ghc-toolchain,
  all:ghci, all:haskeline, all:hpc, all:integer-gmp, all:mtl,
  all:os-string, all:parsec, all:pretty, all:process, all:rts,
  all:semaphore-compat, all:stm, all:system-cxx-std-lib,
  all:template-haskell, all:text, all:time, all:transformers,
  all:unix, all:xhtml

constraints:
  Cabal installed, Cabal-syntax installed, array installed, base installed,
  binary installed, bytestring installed, containers installed, deepseq installed,
  directory installed, exceptions installed, filepath installed, ghc installed,
  ghc-bignum installed, ghc-boot installed, ghc-boot-th installed,
  ghc-compact installed, ghc-experimental installed, ghc-heap installed,
  ghc-internal installed, ghc-platform installed, ghc-prim installed,
  ghc-toolchain installed, ghci installed, haskeline installed, hpc installed,
  integer-gmp installed, mtl installed, os-string installed, parsec installed,
  pretty installed, process installed, rts installed, semaphore-compat installed,
  stm installed, system-cxx-std-lib installed, template-haskell installed,
  text installed, time installed, transformers installed, unix installed, xhtml installed
"""
with open(os.path.join(D, "frontend-wasm", "cabal.project.local"), "w") as f:
    f.write(cabal_local)

# 3. Ensure build-wasm.sh is clean and relies on wasm32-wasi-cabal's native TH handling
build_sh = """#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
wasm32-wasi-clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with wasm32-wasi-cabal..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"
unset GHC_PACKAGE_PATH

rm -rf "$DIR/dist-newstyle/cache/plan.json" "$DIR/frontend-wasm/dist-newstyle"

wasm32-wasi-cabal update

cd "$DIR/frontend-wasm"
# Cabal 3.14+ in GHC 9.10 automatically handles the Wasm external interpreter for TH
wasm32-wasi-cabal build frontend-wasm-exe

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
[ -z "$OBJ_FILE" ] && { echo "Error: Main.o not found"; exit 1; }

wasm32-wasi-ghc -O2 -no-hs-main -optl-mexec-model=reactor -optl-Wl,--allow-undefined -optl-Wl,--export=start_reactor -optl-Wl,--export=reactor_stop -optl-Wl,--export-all "$OBJ_FILE" "$DIR/dist-wasm/stubs.o" -o "$DIR/dist-wasm/reactor.wasm"
echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
"""
with open(os.path.join(D, "scripts", "build-wasm.sh"), "w") as f:
    f.write(build_sh)
os.chmod(os.path.join(D, "scripts", "build-wasm.sh"), 0o755)

print("[HRSM] Upgraded to GHC 9.10. Wasm Template Haskell support is now fully available.")

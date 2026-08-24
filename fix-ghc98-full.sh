#!/usr/bin/env bash
set -euo pipefail
DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Updating flake.nix to use GHC 9.8 for Wasm (required for reflex-dom)..."

# 1. Update flake.nix to use all_9_8
cat > "$DIR/flake.nix" << 'FLAKE_EOF'
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
        # Use GHC 9.8 for Wasm to satisfy reflex-dom's base < 4.22 constraint
        wasmToolchain = ghc-wasm-meta.packages.${system}.all_9_8;

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
            
            echo "[HRSM] Configuring cabal for Wasm compilation..."
            cabal configure \
              --project-dir=frontend-wasm \
              --with-compiler=wasm32-wasi-ghc \
              --with-ghc-pkg=wasm32-wasi-ghc-pkg \
              --with-hsc2hs=wasm32-wasi-hsc2hs \
              --disable-documentation \
              --disable-tests \
              --disable-benchmarks
            
            echo "[HRSM] Building frontend-wasm with Wasm GHC..."
            cabal build frontend-wasm-exe \
              --project-dir=frontend-wasm
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
            echo " - Wasm Compiler: wasm32-wasi-ghc (GHC 9.8) is available"
          '';
        };
      }
    );
}
FLAKE_EOF

# 2. Update build-wasm.sh to match
cat > "$DIR/scripts/build-wasm.sh" << 'BUILD_EOF'
#!/usr/bin/env bash
set -euo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mkdir -p "$DIR/dist-wasm"

echo "[1/3] Compiling C stubs..."
clang -c "$DIR/frontend-wasm/stubs.c" -o "$DIR/dist-wasm/stubs.o"

echo "[2/3] Building frontend-wasm with cabal (GHC 9.8)..."
export HOME="$DIR"
export CABAL_DIR="$DIR/.cabal"
rm -rf "$HOME/.config/cabal"

cabal update

cabal build frontend-wasm-exe \
  --project-dir="$DIR/frontend-wasm" \
  --with-compiler=wasm32-wasi-ghc \
  --with-ghc-pkg=wasm32-wasi-ghc-pkg \
  --with-hsc2hs=wasm32-wasi-hsc2hs

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find "$DIR/dist-newstyle" -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
if [ -z "$OBJ_FILE" ]; then 
  echo "Error: Could not find compiled Main.o"
  exit 1
fi

wasm32-wasi-ghc \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--allow-undefined \
  -optl-Wl,--export=start_reactor \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  "$OBJ_FILE" \
  "$DIR/dist-wasm/stubs.o" \
  -o "$DIR/dist-wasm/reactor.wasm"

echo "[HRSM] Done: $DIR/dist-wasm/reactor.wasm"
BUILD_EOF

chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] fl

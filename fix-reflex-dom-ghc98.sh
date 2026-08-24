#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing reflex-dom base constraint by using GHC 9.8 and isolating frontend cabal.project..."

# 1. Update flake.nix to use GHC 9.8 for Wasm toolchain
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
              --project-file=frontend-wasm/cabal.project \
              --with-compiler=wasm32-wasi-ghc-9.8 \
              --with-ghc-pkg=wasm32-wasi-ghc-pkg-9.8 \
              --with-hsc2hs=wasm32-wasi-hsc2hs-9.8 \
              --disable-documentation \
              --disable-tests \
              --disable-benchmarks
            
            echo "[HRSM] Building frontend-wasm with Wasm GHC..."
            cabal build frontend-wasm:frontend-wasm-exe \
              --project-file=frontend-wasm/cabal.project
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
            echo " - Wasm Compiler: wasm32-wasi-ghc-9.8 is available"
          '';
        };
      }
    );
}
EOF

# 2. Create a frontend-only cabal.project to avoid backend dependency resolution
cat << 'EOF' > "$DIR/frontend-wasm/cabal.project"
packages:
  .
EOF

# 3. Update build-wasm.sh to use the frontend-only cabal.project and GHC 9.8
cat << 'EOF' > "$DIR/scripts/build-wasm.sh"
#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/3] Compiling C stubs..."
# clang is provided by the wasmToolchain
clang -c frontend-wasm/stubs.c -o dist-wasm/stubs.o

echo "[2/3] Building frontend-wasm with cabal (GHC 9.8)..."
export HOME=$PWD
export CABAL_DIR=$PWD/.cabal

cabal update

cabal build frontend-wasm:frontend-wasm-exe \
  --project-file=frontend-wasm/cabal.project \
  --with-compiler=wasm32-wasi-ghc-9.8 \
  --with-ghc-pkg=wasm32-wasi-ghc-pkg-9.8 \
  --with-hsc2hs=wasm32-wasi-hsc2hs-9.8

echo "[3/3] Linking with stubs..."
OBJ_FILE=$(find dist-newstyle -type f -name "Main.o" | grep "frontend-wasm" | head -n 1)
if [ -z "$OBJ_FILE" ]; then
  echo "Error: Could not find compiled Main.o"
  exit 1
fi

wasm32-wasi-ghc-9.8 \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--allow-undefined \
  -optl-Wl,--export=start_reactor \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  "$OBJ_FILE" \
  dist-wasm/stubs.o \
  -o dist-wasm/reactor.wasm

echo "[HRSM] Done: dist-wasm/reactor.wasm"
EOF
chmod +x "$DIR/scripts/build-wasm.sh"

echo "[HRSM] flake.nix, frontend-wasm/cabal.project, and build-wasm.sh updated."
echo "Next steps:"
echo "  1. exit (to leave current nix develop shell)"
echo "  2. nix develop"
echo "  3. ./scripts/build-wasm.sh"

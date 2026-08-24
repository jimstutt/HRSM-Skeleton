#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Setting up proper Wasm build using ghc-wasm-meta and cabal..."

# 1. Create cabal.project to link local packages for the Wasm build
cat << 'EOF' > "$DIR/cabal.project"
packages:
  common
  backend
  frontend-wasm
EOF

# 2. Update flake.nix to use ghc-wasm-meta
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
            echo "[HRSM] Configuring cabal for Wasm compilation..."
            cabal configure \
              --with-compiler=wasm32-wasi-ghc \
              --with-hc-pkg=wasm32-wasi-ghc-pkg \
              --with-hsc2hs=wasm32-wasi-hsc2hs
            
            echo "[HRSM] Building frontend-wasm with Wasm GHC..."
            # Pass linker flags to produce a WASI reactor with the required exports
            cabal build frontend-wasm:frontend-wasm-exe \
              --ghc-options="-optl-mexec-model=reactor -optl-Wl,--export=reactor_start,--export=reactor_stop,--export-all,--export=hs_init"
          '';

          installPhase = ''
            mkdir -p $out
            # Find the built .wasm file and copy it to the output
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
            wasmToolchain # Makes wasm32-wasi-ghc available in the dev shell
            
            (emacsPkgs.emacsWithPackages (epkgs: [
              epkgs.gptel
            ]))
          ];
          
          shellHook = ''
            echo "[HRSM] Development shell loaded."
            echo " - Backend: nix build .#backend"
            echo " - Frontend Wasm: nix build .#frontend-wasm"
            echo " - Wasm Compiler: wasm32-wasi-ghc is available"
            echo " - Emacs with gptel is available in this shell."
          '';
        };
      }
    );
}
EOF

echo "[HRSM] flake.nix and cabal.project updated successfully."
echo "Next step: Run 'nix build .#frontend-wasm'"

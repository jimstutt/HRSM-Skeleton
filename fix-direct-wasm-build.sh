#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing build-wasm.sh and flake.nix for direct wasm32-wasi-ghc compilation..."

# 1. Update build-wasm.sh to use wasm32-wasi-ghc directly
cat << 'EOF' > "$DIR/scripts/build-wasm.sh"
#!/usr/bin/env bash
set -euo pipefail

mkdir -p dist-wasm

echo "[1/2] Compiling and Linking Haskell → WASI reactor"
wasm32-wasi-ghc \
  -O2 \
  -no-hs-main \
  -optl-mexec-model=reactor \
  -optl-Wl,--export=reactor_start \
  -optl-Wl,--export=reactor_stop \
  -optl-Wl,--export-all \
  -optl-Wl,--export=hs_init \
  frontend-wasm/Main.hs \
  -o dist-wasm/reactor.wasm

echo "[2/2] Done: dist-wasm/reactor.wasm"
EOF
chmod +x "$DIR/scripts/build-wasm.sh"

# 2. Update flake.nix to use wasmToolchain and run the script
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

echo "[HRSM] build-wasm.sh and flake.nix updated successfully."
echo "Next step: Run 'nix build .#frontend-wasm'"

{
  description = "NGOLogisticsCG — integrated backend and WASM client (logistics-server + logistics-client)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        # Import your WASM GHC environment correctly
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };

      in {
        # --- Main development shell ---
        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";

          buildInputs = with pkgs; [
            ghc
            cabal-install
            git
            llvmPackages_15.lld
            wasi-sdk
            wasmGhc
          ];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG development shell"
            echo "Using GHC: $(ghc --version 2>/dev/null || echo 'not found')"
            echo "Using Cabal: $(cabal --version 2>/dev/null || echo 'not found')"

            # Setup cabal config path
            export CABAL_CONFIG="${CABAL_CONFIG:-$PWD/cabal.project}"
            echo "Using cabal config: $CABAL_CONFIG"

            # Cross-compilation hints
            export TARGET=wasi32
            export CC=clang
            export LD=wasm-ld
            export AR=llvm-ar
            export NM=llvm-nm
            export RANLIB=llvm-ranlib
          '';
        };

        # --- Define a default package (logistics-server backend) ---
        packages.default = pkgs.haskellPackages.callCabal2nix "ngologisticscg" ./logistics-server { };

        # --- Formatter and checkers ---
        formatter = pkgs.nixpkgs-fmt;
      });
}

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

        # Import local WASM GHC toolchain definition
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };

      in {
        # === DevShell ===
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

            # -- Verify toolchain availability --
            if ! command -v ghc >/dev/null 2>&1; then
              echo "⚠️  GHC not found in PATH!" >&2
            else
              echo "✅ GHC: $(ghc --version)"
            fi

            if ! command -v cabal >/dev/null 2>&1; then
              echo "⚠️  Cabal not found in PATH!" >&2
            else
              echo "✅ Cabal: $(cabal --version | head -n 1)"
            fi

            # -- Setup cabal configuration file safely --
            export CABAL_CONFIG="$${CABAL_CONFIG:-$${PWD}/cabal.project}"
            echo "📦 Using cabal config: $${CABAL_CONFIG}"

            # -- Cross-compilation toolchain setup --
            export TARGET=wasi32
            export CC=clang
            export LD=wasm-ld
            export AR=llvm-ar
            export NM=llvm-nm
            export RANLIB=llvm-ranlib

            echo "🧠 Environment ready for WASM + Haskell builds"
          '';
        };

        # === Package build (default backend app) ===
        packages.default = pkgs.haskellPackages.callCabal2nix "ngologisticscg" ./logistics-server { };

        # === Formatter ===
        formatter = pkgs.nixpkgs-fmt;
      });
}

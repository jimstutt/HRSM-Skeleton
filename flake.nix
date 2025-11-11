{
  description = "NGOLogisticsCG — integrated backend and WASM client (logistics-server + logistics-client)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # --- Native system environment ---
        pkgs = import nixpkgs {
          localSystem = { inherit system; };
        };

        # --- WASM cross-compilation toolchain ---
        wasiPkgs = import nixpkgs {
          localSystem = { inherit system; };
          crossSystem = { config = "wasm32-wasi"; };
        };

        # --- Local wasm GHC derivation ---
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };

      in {
        # === Development shell ===
        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";

          buildInputs = with pkgs; [
            ghc
            cabal-install
            git
            llvmPackages_15.lld
            wasiPkgs.wasi-sdk
            wasmGhc
          ];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG development shell"
            echo "✅ GHC: $(ghc --version)"
            echo "✅ Cabal: $(cabal --version | head -n 1)"

            export CABAL_CONFIG="$${CABAL_CONFIG:-$${PWD}/cabal.project}"
            echo "📦 Using cabal config: $${CABAL_CONFIG}"

            # Cross toolchain
            export TARGET=wasi32
            export CC=clang
            export LD=wasm-ld
            export AR=llvm-ar
            export NM=llvm-nm
            export RANLIB=llvm-ranlib

            echo "🧠 Environment ready for WASM + Haskell builds"
          '';
        };

        # === Main build target (backend) ===
        packages.default = pkgs.haskellPackages.callCabal2nix "ngologisticscg" ./logistics-server { };

        # === Formatter ===
        formatter = pkgs.nixpkgs-fmt;
      });
}


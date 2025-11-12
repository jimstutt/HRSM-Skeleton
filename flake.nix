{
  description = "NGO Logistics development environment with WASI SDK 28.0 and GHC WASM support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            # Override Haskell set to use modern HLS
            (final: prev: {
              haskellPackages = prev.haskell.packages.ghc96.override {
                overrides = self: super: { };
              };
            })
          ];
        };

        # Import your WASM GHC + WASI SDK 28 toolchain
        wasmGhcSet = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };
        wasmGhc = wasmGhcSet.ghcWasmEnv;
      in {
        packages.default = wasmGhc;

        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";

          buildInputs = with pkgs; [
            git
            cabal-install
            haskell.compiler.ghc96
            haskell.packages.ghc96.haskell-language-server
            llvmPackages_15.lld
            wasmGhc
          ];

          shellHook = ''
            echo "✅ NGO Logistics development environment loaded."
            echo "Using WASI SDK 28.0 and GHC WASM environment."
          '';
        };
      });
}

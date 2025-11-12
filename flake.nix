{
  description = "NGO Logistics dev environment (WASI SDK 28.0, GHC WASM)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        wasmGhcSet = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };
        wasmGhc = wasmGhcSet.ghcWasmEnv;
      in {
        packages.default = wasmGhc;

        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";
          buildInputs = with pkgs; [
            git cabal-install llvmPackages_15.lld
            haskell.compiler.ghc96
            haskell.packages.ghc96.haskell-language-server
            wasmGhc
          ];
          shellHook = ''
            echo "✅ NGO Logistics dev shell loaded"
            echo "WASI SDK 28 + GHC WASM env active"
          '';
        };
      });
}

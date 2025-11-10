{
  description = "NGOLogisticsCG — integrated Haskell backend and client build using Nix flakes and GHC";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [];
        };

        # WASM cross-compilation helper
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix {
          inherit (pkgs) stdenv fetchurl gnumake cmake llvmPackages lib writeTextFile;
          pkgsCross = import nixpkgs { crossSystem = { config = "wasm32-wasi"; }; };
        };
      in
      {
        packages.${system}.default = pkgs.haskellPackages.callCabal2nix "NGOLogisticsCG" ./logistics-server { };

        devShells.${system}.default = pkgs.mkShell {
          name = "ngologisticscg-dev";
          buildInputs = with pkgs; [
            cabal-install
            ghc
            haskell-language-server
            hlint
            ormolu
            wasmGhc
          ];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG development shell"
            echo "Using cabal.project from: $PWD"
          '';
        };

        formatter.${system} = pkgs.nixfmt-rfc-style;
      });
}

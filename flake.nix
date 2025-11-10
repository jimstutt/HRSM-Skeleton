{
  description = "NGOLogisticsCG — unified backend and frontend using Nix flakes and GHC/WASM.";

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

        # --- Optional WASM cross-compiler (from pkgs/wasm32-wasi-ghc-full.nix)
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix {
          inherit (pkgs) stdenv fetchurl gnumake cmake llvmPackages lib writeTextFile;
          pkgsCross = import nixpkgs { crossSystem = { config = "wasm32-wasi"; }; };
        };
      in
      {
        # ---- Define buildable package from logistics-server ----
        packages.default = pkgs.haskellPackages.callCabal2nix "NGOLogisticsCG" ./logistics-server { };

        # ---- Developer shell ----
        devShells.default = pkgs.mkShell {
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

        formatter = pkgs.nixfmt-rfc-style;
      });
}


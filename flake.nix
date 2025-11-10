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
        };

        # --- Optional WASM cross-compiler (from pkgs/wasm32-wasi-ghc-full.nix)
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix {
          inherit (pkgs) stdenv fetchurl gnumake cmake llvmPackages lib writeTextFile;
          # Explicitly pass current system to avoid builtins.currentSystem usage
          pkgsCross = import nixpkgs {
            localSystem = { inherit system; };
            crossSystem = { config = "wasm32-wasi"; };
          };
        };
      in {
        # ---- Buildable package ----
        packages.${system}.default =
          pkgs.haskellPackages.callCabal2nix "NGOLogisticsCG" ./logistics-server { };

        # ---- Development shell ----
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

        # ---- Formatter ----
        formatter.${system} = pkgs.nixfmt-rfc-style;
      });
}

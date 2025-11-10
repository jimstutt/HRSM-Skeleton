{
  description = "NGOLogisticsCG — unified backend and frontend (server + client + common)";

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

        # --- WASM cross compiler setup ---
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix {
          inherit (pkgs) stdenv fetchurl gnumake cmake llvmPackages lib writeTextFile;
          pkgsCross = import nixpkgs {
            localSystem = { inherit system; };
            crossSystem = { config = "wasm32-wasi"; };
          };
        };
      in
      {
        # ---- Packages ----
        packages = {
          default = pkgs.haskellPackages.callCabal2nix "NGOLogisticsCG" ./logistics-server { };
        };

        # ---- Development shells ----
        devShells = {
          default = pkgs.mkShell {
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
        };

        # ---- Formatter ----
        formatter = pkgs.nixfmt-rfc-style;
      });
}

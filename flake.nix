{
  description = "NGO Logistics Client + Server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        packages.${system}.default = pkgs.callPackage ./pkgs { };

        devShells.${system}.default = pkgs.mkShell {
          name = "ngologistics-dev";
          buildInputs = with pkgs; [
            git
            cabal-install
            ghc
            nodejs
            yarn
            pkg-config
            zlib
          ];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG dev shell"
            export CABAL_CONFIG="${CABAL_CONFIG:-$PWD/cabal.project}"
            echo "Using cabal config: $CABAL_CONFIG"
          '';
        };
      });
}

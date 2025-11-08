{
  description = "NGOLogisticsCG — Backend, Client, and Common Haskell packages";

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
      in
      {
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "ngologisticscg";
          version = "0.1.0";

          src = ./.;

          buildInputs = with pkgs; [
            haskell.compiler.ghc98
            cabal-install
            sqlite
            openssl
          ];

          buildPhase = ''
            echo "Building NGOLogisticsCG..."
            cabal build all
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp -r dist-newstyle $out/
          '';
        };

        devShells.default = pkgs.mkShell {
          name = "NGOLogisticsCG-dev";

          buildInputs = with pkgs; [
            haskell.compiler.ghc98
            cabal-install
            sqlite
            openssl
            git
            pkg-config
          ];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG dev shell"
            export CABAL_CONFIG="\${CABAL_CONFIG:-$PWD/cabal.project}"
            echo "Using cabal config: $CABAL_CONFIG"
          '';
        };
      });
}

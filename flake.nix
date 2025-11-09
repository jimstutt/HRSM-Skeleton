{
  description = "NGOLogisticsCG full-stack Haskell project with WASM and server components";

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

        # Common dependencies for development
        devTools = with pkgs; [
          ghc
          cabal-install
          haskell-language-server
          hlint
          ormolu
          git
          gnumake
          cmake
          llvmPackages.clang
          nodejs
        ];
      in {
        # Default development shell
        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev-shell";
          buildInputs = devTools;

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG development shell"

            # Use single quotes to prevent Nix interpolation and let Bash handle ${}
            export CABAL_CONFIG='${CABAL_CONFIG:-'"$PWD"'/cabal.project}'
            echo "Using cabal config: $CABAL_CONFIG"

            export PATH=$PWD/scripts:$PATH
            echo "Scripts available in PATH"
          '';
        };

        # CI shell: used for builds and tests
        devShells.ci = pkgs.mkShell {
          name = "ngologisticscg-ci-shell";
          buildInputs = devTools;

          shellHook = ''
            echo "🏗️  CI build shell for NGOLogisticsCG"
            cabal update
            cabal build all
            cabal test all
          '';
        };

        # Optional formatter
        formatter = pkgs.nixpkgs-fmt;
      });
}

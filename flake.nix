{
  description = "NGOLogisticsCG — A Nix flake with Haskell devshell and Cabal support";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        haskellPackages = pkgs.haskellPackages;

      in
      {
        packages.default = haskellPackages.callCabal2nix "NGOLogisticsCG" ./. { };

        devShells.default = pkgs.mkShell {
          name = "NGOLogisticsCG-devshell";

          buildInputs = with pkgs; [
            cabal-install
            ghc
            haskell-language-server
            git
            nixpkgs-fmt
          ];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG dev shell"

            # Use $$ to escape Bash variable expansions in Nix
            export CABAL_CONFIG="$${CABAL_CONFIG:-$${PWD}/cabal.project}"

            echo "Using cabal config: $CABAL_CONFIG"

            if [ -f "$CABAL_CONFIG" ]; then
              echo "Cabal project file found ✅"
            else
              echo "⚠️  No cabal.project file found, creating a minimal one..."
              echo "packages: ." > "$CABAL_CONFIG"
            fi
          '';
        };
      });
}

{
  description = "NGOLogisticsCG Haskell project with Nix flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.haskellPackages.callPackage ./pkgs {};

        devShells.default = pkgs.mkShell {
          name = "ngo-logistics-dev-shell";
          buildInputs = with pkgs; [
            cabal-install
            ghc
            haskell-language-server
            hlint
            ormolu
            git
          ];

          # ✅ Properly escaped shellHook for Nix
          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG dev shell"

            # Use ''$ to escape Bash variables (so Nix doesn’t try to expand them)
            export CABAL_CONFIG="''${CABAL_CONFIG:-''${PWD}/cabal.project}"
            echo "Using cabal config: ''${CABAL_CONFIG}"

            if [ -f "''${CABAL_CONFIG}" ]; then
              echo "✅ Cabal project file found."
            else
              echo "⚠️  No cabal.project found in ''${PWD}"
            fi
          '';
        };
      });
}

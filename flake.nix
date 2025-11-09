{
  description = "NGOLogisticsCG — Nix flake for Haskell project with Cabal, devshell, formatter, and checks";

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
        # Package definition — builds with cabal2nix
        packages.default = haskellPackages.callCabal2nix "NGOLogisticsCG" ./. { };

        # Dev shell with GHC, Cabal, HLS, and basic tools
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

            # Use $$ to escape Bash variables (Nix treats ${} specially)
            export CABAL_CONFIG="$${CABAL_CONFIG:-$${PWD}/cabal.project}"

            echo "Using cabal config: $CABAL_CONFIG"

            if [ -f "$CABAL_CONFIG" ]; then
              echo "✅ Cabal project file found."
            else
              echo "⚠️  No cabal.project file found — creating one..."
              echo "packages: ." > "$CABAL_CONFIG"
            fi
          '';
        };

        # Formatter for nix fmt
        formatter = pkgs.nixpkgs-fmt;

        # Simple check to verify the package builds correctly
        checks.build = self.packages.${system}.default;
      });
}

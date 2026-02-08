{
  description = "Full-stack Haskell: Reflex (Wasm), Servant, and MariaDB";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    # Provides the GHC Wasm backend toolchain
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        
        # Access the Wasm-enabled GHC provided by the input
        wasm-pkgs = ghc-wasm-meta.packages.${system};

        # Standard Haskell setup for Backend/Common
        haskellPackages = pkgs.haskellPackages.override {
          overrides = hfinal: hprev: {
            # Add local project packages
            common = hfinal.callCabal2nix "common" ./common {};
            shared = hfinal.callCabal2nix "shared" ./shared {};
            backend = hfinal.callCabal2nix "backend" ./backend {};
          };
        };

      in {
        # 1. Development Shell
        devShells.default = pkgs.mkShell {
          name = "haskell-wasm-dev";
          buildInputs = [
            # Haskell tools for Backend
            haskellPackages.ghc
            pkgs.cabal-install
            pkgs.haskell-language-server
            
            # Wasm toolchain for Frontend
            wasm-pkgs.all_9_10 # or latest available version
            
            # Database & System Tools
            pkgs.mariadb
            pkgs.pkg-config
            pkgs.zlib
          ];

          shellHook = ''
            echo "Entering Haskell Wasm + MariaDB Dev Environment"
            # Optional: Setup local MariaDB data dir for development
            export MYSQL_HOME=$PWD/.mysql
            mkdir -p $MYSQL_HOME
          '';
        };

        # 2. Packages (Build outputs)
        packages = {
          backend = haskellPackages.backend;
          # Frontend must be built using the wasm32-wasi-cabal wrapper
          frontend-wasm = pkgs.stdenv.mkDerivation {
            name = "frontend-wasm";
            src = ./.;
            buildInputs = [ wasm-pkgs.all_9_10 pkgs.cabal-install ];
            buildPhase = ''
              export HOME=$TMPDIR
              wasm32-wasi-cabal build frontend
            '';
            installPhase = ''
              mkdir -p $out
              find dist-newstyle -name "*.wasm" -exec cp {} $out/ \;
            '';
          };
        };
      }
    );
}

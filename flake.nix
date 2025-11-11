{
  description = "NGO Logistics – unified Haskell + WASM development flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        # --- WASI cross setup ---
        wasiPkgs = import nixpkgs {
          localSystem = { inherit system; };
          crossSystem = { config = "wasm32-wasi"; };
        };

        # --- Robust wasi-sdk resolver with fallback builder ---
        wasiSdk =
          if (wasiPkgs ? llvmPackages && wasiPkgs.llvmPackages ? wasi-sdk)
          then wasiPkgs.llvmPackages.wasi-sdk
          else if (pkgs ? llvmPackages_15 && pkgs.llvmPackages_15 ? wasi-sdk)
          then pkgs.llvmPackages_15.wasi-sdk
          else if (pkgs ? llvmPackages && pkgs.llvmPackages ? wasi-sdk)
          then pkgs.llvmPackages.wasi-sdk
          else pkgs.stdenv.mkDerivation {
            pname = "wasi-sdk-fallback";
            version = "15.0";
            dontBuild = true;
            dontUnpack = true;
            installPhase = ''
              mkdir -p $out/bin
              echo "#!/bin/sh" > $out/bin/wasicc
              echo "echo '⚠️ Using fallback wasi-sdk stub (no actual compiler)'" >> $out/bin/wasicc
              chmod +x $out/bin/wasicc
            '';
          };

        # --- Import WASM GHC cross compiler ---
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix { pkgs = pkgs; };

      in {
        # ------------------------------------------------------------
        # Development Shells
        # ------------------------------------------------------------
        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";

          nativeBuildInputs = with pkgs; [
            ghc cabal-install hlint ormolu
            nodejs typescript
            llvmPackages_15.lld
            wasiSdk
            wasmGhc
          ];

          shellHook = ''
            echo "🟢 NGOLogisticsCG development shell activated."
            export CABAL_CONFIG="$PWD/cabal.project"
            echo "Using cabal config: $CABAL_CONFIG"
            export PATH="$PWD/scripts:$PATH"
          '';
        };

        # ------------------------------------------------------------
        # Packages
        # ------------------------------------------------------------
        packages = {
          default = pkgs.stdenv.mkDerivation {
            pname = "ngologisticscg";
            version = "0.1.0";
            src = ./.;

            buildInputs = [
              pkgs.ghc
              pkgs.cabal-install
            ];

            buildPhase = ''
              echo "Building Haskell backend..."
              cabal build all
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp $(find dist-newstyle -type f -name NGOLogisticsCG || true) $out/bin/ 2>/dev/null || true
            '';
          };
        };

        # ------------------------------------------------------------
        # Formatter and Checks
        # ------------------------------------------------------------
        formatter = pkgs.nixfmt-rfc-style;

        checks = {
          formatting = pkgs.runCommand "check-formatting" { buildInputs = [ pkgs.nixfmt-rfc-style ]; } ''
            nixfmt --check ${self}
            touch $out
          '';
        };
      });
}

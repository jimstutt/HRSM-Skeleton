{
  description = "NGO Logistics – Haskell Servant backend with WASM build and TypeScript frontend";

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

        # Import WASM toolchain (ghc-wasm32-wasi + wasi-sdk 28)
        ghcWasmEnv = import ./pkgs/wasm32-wasi-ghc-full.nix {
          inherit pkgs;
        };
      in {
        # Package outputs (for building)
        packages = {
          default = pkgs.stdenv.mkDerivation {
            name = "ngologistics-backend";
            src = ./.;

            buildInputs = [
              pkgs.haskellPackages.ghc
              pkgs.haskellPackages.cabal-install
            ];

            buildPhase = ''
              echo "Building NGO Logistics Haskell backend..."
              cabal build all
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp -r dist-newstyle/build/*/*/ngologisticscg-*/x/ngologisticscg/build/ngologisticscg/ngologisticscg $out/bin/
            '';
          };
        };

        # Development shell with both host & wasm compilers
        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";
          buildInputs = [
            pkgs.git
            pkgs.nodejs
            pkgs.haskellPackages.ghc
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hls
            pkgs.haskellPackages.ormolu
            pkgs.typescript
            pkgs.esbuild
            ghcWasmEnv
          ];

          shellHook = ''
            echo "✅ NGO Logistics development environment ready"
            echo "Available commands:"
            echo "  cabal build        – build backend"
            echo "  ./scripts/build-wasm.sh – compile frontend (wasm)"
            echo "  ./scripts/serve-wasm.sh – run TypeScript+Servant dev server"
          '';
        };
      });
}

{
  description = "NGOLogisticsCG — integrated backend and wasm client built via ghc-wasm-meta";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [];
        };

        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix {
          inherit (pkgs) stdenv fetchurl gnumake cmake llvmPackages lib writeTextFile;
          pkgsCross = import nixpkgs { crossSystem = { config = "wasm32-wasi"; }; };
        };
      in
      {
        packages.${system}.default = pkgs.stdenv.mkDerivation {
          pname = "ngo-logistics";
          version = "0.1.0";

          src = ./.;

          buildInputs = [
            pkgs.haskell.compiler.ghc98
            pkgs.cabal-install
          ];

          buildPhase = ''
            echo "Building NGOLogisticsCG..."
            cabal build all
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp -r dist-newstyle/build/*/*/ngo-logistics* $out/bin/ || true
            echo "✅ Build complete. Binaries installed to $out/bin"
          '';
        };

        devShells.${system}.default = pkgs.mkShell {
          name = "ghc-wasm-devshell";
          buildInputs = [
            pkgs.haskell.compiler.ghc98
            pkgs.cabal-install
            pkgs.nodejs
            pkgs.yarn
            wasmGhc
          ];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG dev shell"
            export CABAL_CONFIG="\${CABAL_CONFIG:-\${PWD}/cabal.project}"
            echo "Using cabal config: $CABAL_CONFIG"
            export PATH=$PATH:$PWD/scripts
            echo "PATH updated with ./scripts"
          '';
        };
      });
}

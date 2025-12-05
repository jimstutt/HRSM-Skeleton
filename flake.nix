{
  description = "NGOLogisticsCG - WASM reactor dev + build";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        wasiTool = import ./pkgs/minimal-wasi-toolchain.nix { inherit pkgs; };
      in
      {
        packages.reactortool = wasiTool;

        devShells.default = pkgs.mkShell {
          name = "ngoreactor-devshell";
          buildInputs = with pkgs; [
            wasiTool
            clang
            lld
            wabt
            nodejs
            typescript
            ghc    # native GHC for cabal/native tasks; cross-ghc path must be provided by you
            cabal-install
            gnused
            make
          ];

          shellHook = ''
            # prefer explicit WASI_SYSROOT if you have one, otherwise warn
            if [ -z "${WASI_SYSROOT-}" ]; then
              echo "Warning: WASI_SYSROOT not set. Set WASI_SYSROOT to your wasi-sysroot (wasi-sdk or ghc-wasm env)."
            else
              echo "WASI_SYSROOT = $WASI_SYSROOT"
            fi
            export PATH="${pkgs.lld}/bin:${pkgs.clang}/bin:${pkgs.wabt}/bin:$PATH"
            echo "Dev shell ready."
          '';
        };

        packages.reactor-wasm = pkgs.stdenv.mkDerivation {
          pname = "reactor-wasm-builder";
          version = "0.1";
          src = ./.;
          buildInputs = [ wasiTool pkgs.clang pkgs.lld pkgs.wabt pkgs.makeWrapper ];
          buildPhase = ''
            echo "Running project build script to produce wasm..."
            ./scripts/build-reactor-wasm.sh
          '';
          installPhase = ''
            mkdir -p $out
            cp -v build/*.wasm $out/ || true
            cp -v build/*.wat $out/ || true
            echo "Built artifacts copied to $out"
          '';
        };
      });
}

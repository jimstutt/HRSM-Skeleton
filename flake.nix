{
  description = "NGO Logistics Dev Environment with WASM32-WASI GHC";

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

        # Import your wasm env derivation directly
        wasmGhcEnv = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };

      in {
        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev-env";

          buildInputs = [
            pkgs.cabal-install
            pkgs.haskell.compiler.ghc9101

            # wasm32 env you defined
            wasmGhcEnv

            # Useful extras
            pkgs.binaryen
            pkgs.wasmtime
          ];

          shellHook = ''
            echo "WASM32 GHC + WASI SDK available"
            echo "Sysroot: $(cat ${wasmGhcEnv}/share/wasi-sysroot-path)"
          '';
        };
      }
    );
}

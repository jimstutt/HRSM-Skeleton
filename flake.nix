{
  description = "NGO Logistics Haskell + WASM project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        # Import our prebuilt WASM toolchain environment
        wasmGhcEnv = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };
      in {
        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";

          buildInputs = with pkgs; [
            git
            cabal-install
            ghc
            nodejs
            wasmGhcEnv
          ];

          shellHook = ''
            echo "✅ NGO Logistics development environment"
            echo "WASI SDK: $WASI_SDK_PATH"
          '';
        };
      });
}

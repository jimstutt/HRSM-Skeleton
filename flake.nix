{
  description = "NGO Logistics — WebAssembly GHC Development Environment (WASI SDK 28.0)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # Import our custom WASM toolchain
      ghcWasmEnv = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };
    in {
      # ✅ Development shell (interactive)
      devShells.${system}.default = pkgs.mkShell {
        name = "ngologisticscg-dev";

        buildInputs = [
          ghcWasmEnv
          pkgs.git
          pkgs.cabal-install
          pkgs.nodejs_20
          pkgs.typescript
          pkgs.python3
        ];

        shellHook = ''
          echo
          echo "---------------------------------------------"
          echo "✅ NGO Logistics development environment"
          echo "Using WASI SDK 28.0 + GHC WebAssembly toolchain"
          echo "---------------------------------------------"
          echo
        '';
      };

      # ✅ Expose as a package for CI or reuse in other flakes
      packages.${system}.ghc-wasm-env = ghcWasmEnv;

      # Default package alias for convenience
      defaultPackage.${system} = self.packages.${system}.ghc-wasm-env;

      # Optionally provide an app entrypoint (if you want nix run)
      apps.${system}.wasm = {
        type = "app";
        program = "${ghcWasmEnv}/bin/ghc";
      };
    };
}

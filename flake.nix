{
  description = "NGO Logistics — WebAssembly GHC Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      # Import our prebuilt WASM GHC environment
      ghcWasmEnv = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        name = "ngologisticscg-dev";

        # The key environment we’ve just built
        buildInputs = [
          ghcWasmEnv
        ];

        shellHook = ''
          echo
          echo "---------------------------------------------"
          echo "✅ NGO Logistics development environment"
          echo "Using WASI SDK 28.0 and GHC WebAssembly tools"
          echo "---------------------------------------------"
          echo
        '';
      };
    };
}

{
  description = "NGOLogisticsCG full-stack Haskell + WASM project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Import the WASM GHC environment
        wasmGhc = import ./pkgs/wasm32-wasi-ghc-full.nix { inherit pkgs; };
      in
      {
        packages.default = pkgs.haskellPackages.callCabal2nix "NGOLogisticsCG"
          ./logistics-server {};

        devShells.default = pkgs.mkShell {
          name = "ngologisticscg-dev";

          buildInputs = with pkgs; [
            git
            cabal-install
            ghc
            llvmPackages_15.lld
            nodejs
            python3
            wasmGhc # <- this is a mkShell, so we use its inputs below
          ] ++ wasmGhc.buildInputs or [];

          shellHook = ''
            echo "🧩 Entered NGOLogisticsCG development shell"
            echo "Cabal config: $PWD/cabal.project"
            echo "Using GHC: $(ghc --version || true)"
          '';
        };
      });
}

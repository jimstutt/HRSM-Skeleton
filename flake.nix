{
  description = "NGOLogisticsCG WASM/Web + DevShell";

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

        # Import your custom WASM-enabled GHC environment
        ghcWasmEnv = pkgs.callPackage ./pkgs/wasm32-wasi-ghc-full.nix { };
      in
      {

        # -------------------------------------
        # Development shell with GHC WASM toolset
        # -------------------------------------
        devShells.default = pkgs.mkShell {
          name = "NGOLogisticsCG";

          buildInputs = [
            # Your custom WASM GHC env
            ghcWasmEnv

            # Obelisk (from upstream nixpkgs)
            pkgs.obelisk

            # Web & WASM toolchain
            pkgs.nodejs_20
            pkgs.wasm-pack
            pkgs.binaryen
            pkgs.emscripten
            pkgs.llvm
            pkgs.clang
            pkgs.lld
            pkgs.wabt

            # Utilities
            pkgs.git
            pkgs.curl
            pkgs.wget
            pkgs.typescript
            pkgs.vim
          ];

          shellHook = ''
            echo "✔ NGOLogisticsCG WASM + Obelisk DevShell Active"
            echo "Use: ob run  |  ./scripts/build-reactor.sh"
          '';
        };

        # -------------------------------------
        # Exported packages
        # -------------------------------------
        packages = {
          ghc-wasm32-wasi-env = ghcWasmEnv;
        };

        # -------------------------------------
        # nix run .  launches local dev server
        # -------------------------------------
        apps.default = {
          type = "app";
          program = "${pkgs.bash}/bin/bash -c \"cd $PWD && npm run dev\"";
        };

      });
}

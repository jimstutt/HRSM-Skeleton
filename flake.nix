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
          buildInputs = with pkgs; [
          #   reflex
            obelisk
            nodejs_20
            wasm-pack
            binaryen
            emscripten
            llvm
            clang
            lld
            wabt
            git
            curl
            wget
            typescript
            vim

         # ✔ Correct
            (self.packages.${system}.ghc-wasm32-wasi-full)
         ];

     shellHook = ''
       export WASI_SYSROOT="${self.packages.${system}.ghc-wasm32-wasi-full}/wasi-sdk/share/wasi-sysroot"
       echo "WASI_SYSROOT=$WASI_SYSROOT"
       '';
     };

        # -------------------------------------
        # Exported packages
        # -------------------------------------
        packages = {
          ghc-wasm32-wasi-full =
            import ./pkgs/wasm32-wasi-ghc-full.nix {
              inherit pkgs;
            };
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

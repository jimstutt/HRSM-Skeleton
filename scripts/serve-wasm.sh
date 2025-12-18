{
  description = "NGOL-CG – GHC WASM (reactor) + Reflex + Servant + sqlite-simple";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";

    ghc-wasm-meta = {
      url = "github:WebAssembly/ghc-wasm-meta";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        wasm = ghc-wasm-meta.packages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          name = "NGOL-CG-devshell";

          buildInputs = [
            wasm.wasm32-wasi-ghc
            wasm.wasi-sdk
            pkgs.nodejs_20
            pkgs.sqlite
            pkgs.pkg-config
          ];

          shellHook = ''
            export WASI_SYSROOT=${wasm.wasi-sdk}/share/wasi-sysroot
            export PATH=${wasm.wasm32-wasi-ghc}/bin:$PATH

            echo "========================================"
            echo "NGOL-CG WASM dev shell ready"
            echo "GHC: $(which wasm32-wasi-ghc)"
            echo "WASI_SYSROOT=$WASI_SYSROOT"
            echo "========================================"
          '';
        };
      });
}

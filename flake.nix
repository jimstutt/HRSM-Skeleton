{
  description = "NGO Logistics — WASM + Haskell Development Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Import full WASM GHC + WASI SDK set
        wasmGhcSet = import ./pkgs/wasm32-wasi-ghc-full.nix {
          inherit pkgs;
        };

        wasmGhc = wasmGhcSet.ghcWasmEnv;
      in
      {

        # --- Development shell ----------------------------------------------
        devShell = pkgs.mkShell {
          name = "ngologisticscg-dev-env";

          buildInputs = [
            pkgs.haskell.compiler.ghc94
            pkgs.cabal-install
            wasmGhc
            pkgs.clang
            pkgs.lld
            pkgs.wabt
            pkgs.nodejs
            pkgs.wasm-pack
            pkgs.python3
            pkgs.git
          ];

          # For debugging WASM builds
          shellHook = ''
            echo "=== NGO Logistics WASM Environment Loaded ==="
            echo "  clang version: $(clang --version | head -n 1)"
            echo "  wasm-ld path:  $(which wasm-ld)"
            echo "  sysroot:       $(cat ${wasmGhc}/share/wasi-sysroot-path)"
            echo "================================================"
          '';
        };

        # --- App or utility binary (optional runnable) -----------------------
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "ngologisticscg-placeholder";
          version = "0.1";
          src = ./.;

          installPhase = ''
            mkdir -p $out/bin
            echo "#!/bin/sh" > $out/bin/ngologisticscg
            echo "echo NGO Logistics Build OK" >> $out/bin/ngologisticscg
            chmod +x $out/bin/ngologisticscg
          '';
        };

        # Allow `nix run .#wasm`
        apps.wasm = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/ngologisticscg";
        };

      });
}

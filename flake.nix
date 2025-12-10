{
  description = "NGOLogisticsCG WASM + DevShell (WASI SDK via local tarball)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # Import the local WASI SDK derivation
        wasiSdk = (import ./pkgs/wasm32-wasi-sdk.nix { inherit pkgs; });

        # sysroot path produced by our local WASI SDK
        wasiSysroot = "${wasiSdk}/wasi-sdk/share/wasi-sysroot";

        devShell = pkgs.mkShell {
          name = "NGOLogisticsCG-devshell";

          buildInputs = with pkgs; [
            nodejs_20
            wasm-pack
            binaryen
            emscripten
            llvmPackages.clang
            lld
            wabt
            git
            curl
            wget
            typescript
            vim
            wasiSdk
          ];

          # IMPORTANT: escape $ so Nix does not evaluate ${WASI_SYSROOT}
          shellHook = ''
            export WASI_SYSROOT="${wasiSysroot}"
            echo "[NGOLogisticsCG devshell] WASI_SYSROOT=$WASI_SYSROOT"
            echo "To build reactor WASM: ./scripts/build-reactor-wasi.sh"
          '';
        };
      in {
        devShells.default = devShell;

        packages = {
          wasi-sdk = wasiSdk;
        };

        apps.default = {
          type = "app";
          program = "${pkgs.bash}/bin/bash -c \"echo Run './scripts/build-reactor-wasi.sh' inside nix develop\"";
        };
      }
    );
}

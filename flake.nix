{
  description = "NGOL-CG – Bootstrap flake (native + WASM toolchains)";

inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  flake-utils.url = "github:numtide/flake-utils";

  ghc-wasm-meta = {
    url = "git+https://github.com/input-output-hk/ghc-wasm-meta.git?ref=main";
    inputs.nixpkgs.follows = "nixpkgs";
  };
};

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ghc-wasm-meta
    }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = false;
        };

        # -----------------------------
        # Native Haskell toolchain
        # -----------------------------
        nativeHaskell = pkgs.haskell.packages.ghc965;

        nativeTools = with pkgs; [
          git
          sqlite
          sqlitebrowser
          pkg-config
          cacert
        ];

        # -----------------------------
        # WASM toolchain (explicit)
        # -----------------------------
        wasmPkgs =
          ghc-wasm-meta.packages.${system};

        wasmGhc =
          wasmPkgs.ghc;

        wasmTools = with pkgs; [
          binaryen
          wabt
        ];

      in
      {
        # =============================
        # Packages
        # =============================
        packages = {
          native-ghc = nativeHaskell.ghc;
          wasm-ghc   = wasmGhc;
        };

        # =============================
        # Development shells
        # =============================
        devShells = {
          native = pkgs.mkShell {
            name = "ngol-cg-native";

            buildInputs =
              [ nativeHaskell.ghc ]
              ++ nativeTools;

            shellHook = ''
              echo "NGOL-CG native shell"
              echo "GHC: $(${nativeHaskell.ghc}/bin/ghc --version)"
              echo "SQLite: $(sqlite3 --version)"
            '';
          };

          wasm = pkgs.mkShell {
            name = "ngol-cg-wasm";

            buildInputs =
              [ wasmGhc ]
              ++ wasmTools;

            shellHook = ''
              echo "NGOL-CG WASM shell"
              echo "GHC (wasm): $(${wasmGhc}/bin/ghc --version)"
            '';
          };
        };

        # =============================
        # Checks (flake hygiene)
        # =============================
        checks = {
          native-ghc = pkgs.runCommand "check-native-ghc" { } ''
            ${nativeHaskell.ghc}/bin/ghc --version > $out
          '';

          wasm-ghc = pkgs.runCommand "check-wasm-ghc" { } ''
            ${wasmGhc}/bin/ghc --version > $out
          '';
        };
      }
    );
}

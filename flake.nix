{
  description = "HSMWasm: Haskell Servant Mariadb Wasm App";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; config = { allowBroken = true; }; };
        haskellPkgs = pkgs.haskellPackages;
        # GHC 9.10 has base-4.20 includes Wasm TH support
        wasmToolchain = ghc-wasm-meta.packages.${system}.all_9_10;
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend { common = commonPkg; };
      in {
        packages = {
          ts-types = pkgs.callPackage ./nix/generate-ts.nix { inherit commonPkg; };
          inherit commonPkg backendPkg;
          common = commonPkg;
          backend = backendPkg;
          default = backendPkg;
        };
        devShells.default = pkgs.mkShell {
          buildInputs = [ 
            haskellPkgs.cabal-install 
            haskellPkgs.haskell-language-server 
            pkgs.mariadb 
            pkgs.pkg-config 
            pkgs.wasmtime
            wasmToolchain 
          ];
          shellHook = "echo '[HRSM] Dev shell loaded. Wasm Compiler: wasm32-wasi-ghc (GHC 9.10)'";
        };
      }
    );
}
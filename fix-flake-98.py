import os
D = "/home/jimstutt/Dev/HRSM-Skeleton"
flake = """{
  description = "HRSM-Skeleton";
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
        wasmToolchain = ghc-wasm-meta.packages.${system}.all_9_8;
        commonPkg = haskellPkgs.callCabal2nix "common" ./common {};
        backendPkg = haskellPkgs.callCabal2nix "backend" ./backend { common = commonPkg; };
      in {
        packages = {
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
          shellHook = "echo '[HRSM] Dev shell loaded. Wasm Compiler: wasm32-wasi-ghc (GHC 9.8)'";
        };
      }
    );
}"""
with open(os.path.join(D, "flake.nix"), "w") as f:
    f.write(flake)
print("[HRSM] flake.nix updated to use all_9_8.")

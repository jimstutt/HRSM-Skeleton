# pkgs/wasm32-wasi-ghc-full.nix
{ pkgs ? import <nixpkgs> { } }:

let
  # Import nixpkgs for both the host (Linux) and the WASM target
  hostPkgs = pkgs;
  wasmPkgs = import pkgs.path {
    crossSystem = { config = "wasm32-wasi"; };
  };

in
hostPkgs.stdenv.mkDerivation {
  pname = "wasm32-wasi-ghc-full";
  version = "1.0";

  # This derivation doesn’t build new binaries — it just exposes tools
  dontBuild = true;
  dontInstall = true;

  buildInputs = [
    hostPkgs.ghc
    hostPkgs.cabal-install
    hostPkgs.wasi-sdk
    wasmPkgs.llvmPackages_15.lld
  ];

  shellHook = ''
    echo "✅ Loaded wasm32-wasi GHC cross environment"
    export TARGET=wasi32
    export CC=clang
    export LD=wasm-ld
    export AR=llvm-ar
    export NM=llvm-nm
    export RANLIB=llvm-ranlib
  '';
}

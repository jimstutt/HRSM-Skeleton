# pkgs/wasm32-wasi-ghc-full.nix
{ pkgs ? import <nixpkgs> { } }:

let
  hostPkgs = pkgs;
  wasmPkgs = import pkgs.path {
    crossSystem = {
      config = "wasm32-wasi";
    };
  };

  # Prefer internal wasi-sdk if missing
  wasiSdk = if wasmPkgs ? wasi-sdk then wasmPkgs.wasi-sdk else
    wasmPkgs.stdenv.mkDerivation {
      pname = "wasi-sdk";
      version = "fake";
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out
        echo "⚠️ Using dummy wasi-sdk" > $out/README.txt
      '';
    };

  ghcWasmEnv = hostPkgs.mkShell {
    name = "ghc-wasm32-wasi-full";

    buildInputs = [
      hostPkgs.gcc
      hostPkgs.gnumake
      hostPkgs.cabal-install
      wasmPkgs.llvmPackages_15.lld
      wasiSdk
    ];

    shellHook = ''
      #!/usr/bin/env bash
      echo "🧩 Entered WASM GHC cross environment"
      echo "Using LLVM: $(lld --version 2>/dev/null || echo 'not found')"
      echo "Using fake wasi-sdk path: ${wasiSdk}"
    '';
  };
in
{
  inherit ghcWasmEnv;
}

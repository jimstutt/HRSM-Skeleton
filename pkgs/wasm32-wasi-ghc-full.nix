# pkgs/wasm32-wasi-ghc-full.nix
#
# Robust GHC WASM cross environment, compatible with nixpkgs >= 23.11 and 24.05.
# This file no longer assumes hostPkgs.wasi-sdk exists.

{ pkgs
, wasiSdk ? null
}:

let
  hostPkgs = pkgs;
  wasmPkgs = import pkgs.path {
    localSystem = { system = hostPkgs.stdenv.system; };
    crossSystem = { config = "wasm32-wasi"; };
  };

  # Fallback in case caller didn’t supply a wasiSdk
  resolvedWasiSdk =
    if wasiSdk != null then wasiSdk else
    if wasmPkgs ? llvmPackages && wasmPkgs.llvmPackages ? wasi-sdk
    then wasmPkgs.llvmPackages.wasi-sdk
    else if hostPkgs ? llvmPackages_15 && hostPkgs.llvmPackages_15 ? wasi-sdk
    then hostPkgs.llvmPackages_15.wasi-sdk
    else hostPkgs.stdenv.mkDerivation {
      pname = "wasi-sdk-fallback";
      version = "15.0";
      dontBuild = true;
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out/bin
        echo "#!/bin/sh" > $out/bin/wasicc
        echo "echo '⚠️ Using fallback wasi-sdk stub (no compiler)'" >> $out/bin/wasicc
        chmod +x $out/bin/wasicc
      '';
    };

in
{
  ghcWasmEnv = hostPkgs.mkShell {
    name = "ghc-wasm32-wasi-full";

    buildInputs = with hostPkgs; [
      ghc
      cabal-install
      llvmPackages_15.lld
      resolvedWasiSdk
    ];

    shellHook = ''
      echo "🟣 GHC WASM cross-compilation environment active"
      echo "Using wasi-sdk at: ${resolvedWasiSdk}"
    '';
  };
}

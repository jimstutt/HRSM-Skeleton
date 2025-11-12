{ pkgs }:

let
  version = "28.0";
  sha256 = "0p93lf8qkwww9k4fn7qisjshvyf9fbipmr1avmrzxzkapxhwcpf4";

  wasi-sdk = pkgs.stdenv.mkDerivation {
    name = "wasi-sdk-${version}";
    src = pkgs.fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${version}/wasi-sdk-${version}.0-x86_64-linux.tar.gz";
      sha256 = sha256;
    };
    nativeBuildInputs = [ pkgs.autoPatchelfHook pkgs.gcc ];
    unpackPhase = "tar xzf $src";
    installPhase = ''
      mkdir -p $out
      cp -r wasi-sdk-${version}.0/* $out/
    '';
  };

  ghcWasmEnv = pkgs.stdenv.mkDerivation {
    name = "ghc-wasm32-wasi-env-9.10.1";
    nativeBuildInputs = [
      pkgs.gcc
      pkgs.llvmPackages_15.lld
      pkgs.cabal-install
      wasi-sdk
    ];
    buildCommand = ''
      mkdir -p $out/bin
      echo "#!/usr/bin/env bash" > $out/bin/ghc-wasm
      echo "echo 'ghc-wasm (WASI SDK ${version}) environment active'" >> $out/bin/ghc-wasm
      chmod +x $out/bin/ghc-wasm
    '';
  };
in {
  inherit ghcWasmEnv wasi-sdk;
}

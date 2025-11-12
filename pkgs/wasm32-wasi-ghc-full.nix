{ pkgs ? import <nixpkgs> { } }:

let
  # Fetch prebuilt WASI SDK 28.0
  prebuiltWasiSdk = pkgs.stdenv.mkDerivation {
    pname = "wasi-sdk";
    version = "28.0";

    src = pkgs.fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-28/wasi-sdk-28.0-x86_64-linux.tar.gz";
      sha256 = "0p93lf8qkwww9k4fn7qisjshvyf9fbipmr1avmrzxzkapxhwcpf4";
    };

    dontConfigure = true;
    dontBuild = true;
    installPhase = ''
      mkdir -p $out
      tar -xzf $src --strip-components=1 -C $out
    '';
  };

  # Define the wasm32-wasi target environment
  wasmPkgs = import pkgs.path {
    system = "x86_64-linux"; # Host system for the toolchain
    crossSystem = {
      config = "wasm32-wasi";
    };
  };

in
pkgs.stdenv.mkDerivation rec {
  pname = "ghc-wasm32-wasi-env";
  version = "9.10.1";

  # Provide development environment for GHC targeting wasm32-wasi
  buildInputs = with pkgs; [
    ghc
    cabal-install
    llvmPackages_15.lld
    binaryen
    nodejs
    wasmtime
    wabt
  ];

  # Include the prebuilt WASI SDK paths
  nativeBuildInputs = [
    prebuiltWasiSdk
  ];

  # Set up environment variables
  shellHook = ''
    export WASI_SDK_PATH=${prebuiltWasiSdk}
    export PATH=$WASI_SDK_PATH/bin:$PATH
    export CC=clang
    export CXX=clang++
    export LD=wasm-ld
    export AR=llvm-ar
    export NM=llvm-nm
    export RANLIB=llvm-ranlib
    export CFLAGS="--target=wasm32-wasi --sysroot=$WASI_SDK_PATH/share/wasi-sysroot"
    export LDFLAGS="--target=wasm32-wasi --sysroot=$WASI_SDK_PATH/share/wasi-sysroot"
    echo "✅ WASI SDK environment (v28.0) ready for wasm32-wasi builds."
  '';

  # Prevent actual building
  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  dontInstall = true;
}

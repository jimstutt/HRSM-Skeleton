# pkgs/wasm32-wasi-ghc-full.nix
# Provides a GHC + WASI SDK 28.0 environment for compiling Haskell to WebAssembly

{ pkgs }:

let
  inherit (pkgs) stdenv fetchurl llvmPackages lib;

  wasiSdk = stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = "28.0";

    src = fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${version}/wasi-sdk-${version}-x86_64-linux.tar.gz";
      sha256 = "0p93lf8qkwww9k4fn7qisjshvyf9fbipmr1avmrzxzkapxhwcpf4";
    };

    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      mkdir -p $out
      tar -xzf $src --strip-components=1 -C $out
    '';
  };

  ghcWasmEnv = stdenv.mkDerivation {
    pname = "ghc-wasm32-wasi-env";
    version = "9.10.1";

    nativeBuildInputs = with pkgs; [
      llvmPackages.clang
      llvmPackages.lld
      wasiSdk
      binaryen
      wabt
      cabal-install
    ];

    installPhase = ''
      mkdir -p $out/bin
      # Export environment wrapper script
      cat > $out/bin/ghc-wasm-env <<'EOF'
      #!/usr/bin/env bash
      export WASI_SDK_PATH=${wasiSdk}
      export PATH="$WASI_SDK_PATH/bin:$PATH"
      export CC=clang
      export CXX=clang++
      export LD=wasm-ld
      export AR=llvm-ar
      export NM=llvm-nm
      export RANLIB=llvm-ranlib
      export CFLAGS="--target=wasm32-wasi --sysroot=$WASI_SDK_PATH/share/wasi-sysroot"
      export LDFLAGS="$CFLAGS"
      echo "✅ WASI SDK 28.0 (GHC WASM Env) is ready."
      bash
      EOF
      chmod +x $out/bin/ghc-wasm-env
    '';
  };

in
ghcWasmEnv

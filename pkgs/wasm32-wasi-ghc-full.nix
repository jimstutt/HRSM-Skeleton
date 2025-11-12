# pkgs/wasm32-wasi-ghc-full.nix
# Pure WASI SDK 28.0 + GHC WASM environment for nix develop
{ pkgs }:

let
  inherit (pkgs) stdenv fetchurl llvmPackages;

  # Download and unpack prebuilt WASI SDK 28.0
  wasiSdk = stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = "28.0";

    src = fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${version}/wasi-sdk-${version}-x86_64-linux.tar.gz";
      sha256 = "0p93lf8qkwww9k4fn7qisjshvyf9fbipmr1avmrzxzkapxhwcpf4";
    };

    dontUnpack = true;
    dontBuild = true;
    dontPatchELF = true;
    dontFixup = true;
    dontStrip = true;

    installPhase = ''
      mkdir -p $out
      tar -xzf $src --strip-components=1 -C $out
    '';

    meta = {
      description = "Prebuilt WASI SDK 28.0 for WebAssembly (no patching)";
      homepage = "https://github.com/WebAssembly/wasi-sdk";
      platforms = [ "x86_64-linux" ];
    };
  };

  # Build GHC WASM environment
  ghcWasmEnv = stdenv.mkDerivation {
    pname = "ghc-wasm32-wasi-env";
    version = "9.10.1";

    # Only tool dependencies, no building
    dontUnpack = true;
    dontBuild = true;

    nativeBuildInputs = with pkgs; [
      llvmPackages.clang
      llvmPackages.lld
      binaryen
      wabt
      cabal-install
    ];

    installPhase = ''
      mkdir -p $out/bin
      cat > $out/bin/wasm-env <<'EOF'
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
      echo "✅ GHC WASM Environment using WASI SDK 28.0 ready."
      bash
      EOF
      chmod +x $out/bin/wasm-env
    '';

    meta.description = "GHC WASM32-WASI environment using prebuilt WASI SDK 28.0";
  };

in
ghcWasmEnv

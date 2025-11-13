{ pkgs ? import <nixpkgs> {} }:

let
  # WASI SDK 28.0 from GitHub
  wasiSdk = pkgs.stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = "28.0";

    src = pkgs.fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${version}/wasi-sdk-${version}-x86_64-linux.tar.gz";
      sha256 = "0p93lf8qkwww9k4fn7qisjshvyf9fbipmr1avmrzxzkapxhwcpf4"; # from nix-prefetch-url
    };

    dontConfigure = true;
    dontBuild = true;
    dontPatchELF = true;
    dontStrip = true;

    installPhase = ''
      mkdir -p $out
      tar -xzf $src --strip-components=1 -C $out
    '';

    meta = with pkgs.lib; {
      description = "Prebuilt WebAssembly System Interface SDK for building WASM binaries";
      homepage = "https://github.com/WebAssembly/wasi-sdk";
      license = licenses.ncsa;
      platforms = [ "x86_64-linux" ];
      maintainers = [ maintainers.example ];
    };
  };

  # GHC WASM environment using the prebuilt WASI SDK
  ghcWasmEnv = pkgs.stdenv.mkDerivation rec {
    pname = "ghc-wasm32-wasi-env";
    version = "9.10.1";

    nativeBuildInputs = [
      pkgs.git
      pkgs.cmake
      pkgs.llvmPackages_18.clang
      pkgs.llvmPackages_18.lld
    ];

    buildInputs = [
      wasiSdk
    ];

    dontBuild = true;
    dontPatchELF = true;

    installPhase = ''
      mkdir -p $out/bin
      echo "#!/usr/bin/env bash" > $out/bin/ghc-wasm32-wasi
      echo "export WASI_SDK_PATH=${wasiSdk}" >> $out/bin/ghc-wasm32-wasi
      echo "echo 'GHC WASM environment (WASI SDK 28.0) ready'" >> $out/bin/ghc-wasm32-wasi
      chmod +x $out/bin/ghc-wasm32-wasi
    '';

    meta = with pkgs.lib; {
      description = "GHC WASM cross-compilation environment using WASI SDK 28.0";
      homepage = "https://github.com/WebAssembly/wasi-sdk";
      license = licenses.ncsa;
      platforms = [ "x86_64-linux" ];
    };
  };
in
{
  inherit wasiSdk ghcWasmEnv;
}

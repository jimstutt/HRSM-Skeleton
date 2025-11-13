{ pkgs ? import <nixpkgs> {} }:

let
  wasiSdkVersion = "28.0";
  wasiSdkHash = "0p93lf8qkwww9k4fn7qisjshvyf9fbipmr1avmrzxzkapxhwcpf4"; # verified from previous build
  srcUrl = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${wasiSdkVersion}/wasi-sdk-${wasiSdkVersion}-x86_64-linux.tar.gz";

  wasiSdk = pkgs.stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = wasiSdkVersion;
    src = pkgs.fetchurl {
      url = srcUrl;
      sha256 = wasiSdkHash;
    };

    dontPatchELF = true;
    dontAutoPatchelf = true;
    dontStrip = true;

    nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];

    unpackPhase = ''
      tar xf $src
    '';

    installPhase = ''
      mkdir -p $out
      cp -r wasi-sdk-${version}-x86_64-linux/* $out/
    '';

    meta = with pkgs.lib; {
      description = "WASI SDK ${version} (toolchain for WebAssembly System Interface)";
      homepage = "https://github.com/WebAssembly/wasi-sdk";
      license = licenses.ncsa;
      platforms = [ "x86_64-linux" ];
    };
  };

  # Define a GHC wasm32-wasi environment built with that SDK
  ghcWasmEnv = pkgs.mkShell {
    name = "ghc-wasm32-wasi-env";
    buildInputs = [ wasiSdk pkgs.ghc pkgs.cabal-install pkgs.wasm-tools ];
    shellHook = ''
      export WASI_SDK_PATH=${wasiSdk}
      export PATH=$WASI_SDK_PATH/bin:$PATH
      echo "✅ GHC WASM environment ready (WASI SDK ${wasiSdkVersion})"
    '';
  };
in
{
  inherit wasiSdk ghcWasmEnv;
}

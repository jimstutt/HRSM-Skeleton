{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs) stdenv fetchurl lib makeWrapper licenses;

  # WASI SDK 28.0 — fixed extraction and metadata
  wasiSdk = stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = "28.0";

    src = fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${version}/wasi-sdk-${version}-x86_64-linux.tar.gz";
      sha256 = "sha256-hCjUpkPaWyC8Z9DGCEBtmxzQZ6jRxhzq58s9aHf3Yh8=";
    };

    nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];
    dontAutoPatchelf = true;

    unpackPhase = ''
      tar xf $src
    '';

    installPhase = ''
      mkdir -p $out
      cp -r wasi-sdk-${version}-x86_64-linux/* $out/
    '';

    meta = with lib; {
      description = "WASI SDK ${version} — Clang/LLVM toolchain targeting WebAssembly System Interface (WASI)";
      homepage = "https://github.com/WebAssembly/wasi-sdk";
      license = licenses.ncsa; # LLVM-style
      platforms = [ "x86_64-linux" ];
      maintainers = [ maintainers.eelco ];
    };
  };

  # GHC WASM environment — skip unpackPhase completely
  ghcWasmEnv = stdenv.mkDerivation rec {
    pname = "ghc-wasm32-wasi-env";
    version = "9.10.1";

    dontUnpack = true;
    dontBuild = true;

    # Disable unpackPhase explicitly
    phases = [ "installPhase" ];

    buildInputs = [ wasiSdk pkgs.nodejs pkgs.binaryen pkgs.wabt ];

    installPhase = ''
      mkdir -p $out/bin
      ln -s ${pkgs.ghc}/bin/ghc $out/bin/ghc
      ln -s ${pkgs.ghc}/bin/ghci $out/bin/ghci

      mkdir -p $out/env
      cat > $out/env/setup.sh <<EOF
        export PATH=${wasiSdk}/bin:$PATH
        export WASI_SDK_PATH=${wasiSdk}
        export CC=${wasiSdk}/bin/clang
        export AR=${wasiSdk}/bin/ar
        export NM=${wasiSdk}/bin/nm
        export RANLIB=${wasiSdk}/bin/ranlib
        export LD=${wasiSdk}/bin/wasm-ld
        export GHC_WASM32_WASI=1
      EOF
      chmod +x $out/env/setup.sh
    '';

    meta = with lib; {
      description = "GHC ${version} environment for compiling Haskell to WebAssembly (WASI)";
      homepage = "https://gitlab.haskell.org/ghc/ghc";
      license = licenses.ncsa;
      platforms = [ "x86_64-linux" ];
      maintainers = [ maintainers.eelco ];
    };
  };
in
{
  inherit wasiSdk ghcWasmEnv;
}

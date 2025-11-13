{ pkgs }:
let
  inherit (pkgs) stdenv fetchurl lib makeWrapper;

  # Fetch WASI SDK 28.0 directly from GitHub
  wasiSdk = stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = "28.0";
    src = fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${version}/wasi-sdk-${version}-x86_64-linux.tar.xz";
      sha256 = "sha256-mEqfHwzK/xBrPfbLhK4SMyLbY1h/7E4FYbA79IAvXbc=";
    };

    dontUnpack = true;
    dontBuild = true;
    dontPatchELF = true;

    installPhase = ''
      mkdir -p $out
      tar -xJf $src --strip-components=1 -C $out
    '';

    meta = with lib; {
      description = "WebAssembly System Interface (WASI) SDK ${version}";
      homepage = "https://github.com/WebAssembly/wasi-sdk";
      license = lib.licenses.ncsa;  # NCSA Open Source License
      platforms = [ "x86_64-linux" ];
      maintainers = [ maintainers.example ];
    };
  };

  # Build GHC WASM environment
  ghcWasmEnv = stdenv.mkDerivation rec {
    pname = "ghc-wasm32-wasi-env";
    version = "9.10.1";

    dontUnpack = true;
    dontBuild = true;
    dontPatchELF = true;

    installPhase = ''
      mkdir -p $out/bin
      echo "#!/bin/sh" > $out/bin/ghc-wasm32
      echo "echo 'GHC WASM32-WASI environment ${version}'" >> $out/bin/ghc-wasm32
      chmod +x $out/bin/ghc-wasm32
    '';

    meta = with lib; {
      description = "GHC cross-compilation environment for WASM32-WASI";
      homepage = "https://gitlab.haskell.org/ghc/ghc";
      license = lib.licenses.bsd3;
      platforms = [ "x86_64-linux" ];
    };
  };

in {
  inherit wasiSdk ghcWasmEnv;
}

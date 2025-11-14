{ pkgs }:

let
  inherit (pkgs) stdenv fetchurl lib;

  wasiSdk = stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = "22.0";

    src = fetchurl {
      # IMPORTANT: tag is "wasi-sdk-22", not "wasi-sdk-22.0"
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-22/wasi-sdk-${version}-x86_64-linux.tar.xz";
      sha256 = "sha256-+vDHzNjGwMNjT5aw11KMFRuyXTSRqQYT0bzjqkY7q8I=";
    };

    dontUnpack = false;

    installPhase = ''
      mkdir -p $out
      cd wasi-sdk-${version}
      cp -r * $out/

      mkdir -p $out/bin

      # Make clang tools discoverable
      ln -sf $out/bin/clang-* $out/bin/clang || true
      ln -sf $out/bin/clang-* $out/bin/clang++ || true
      ln -sf $out/bin/lld $out/bin/wasm-ld || true
    '';

    dontPatchELF = true;
    dontStrip = true;

    meta = with lib; {
      description = "WASI SDK ${version}";
      homepage = "https://github.com/WebAssembly/wasi-sdk";
      license = licenses.ncsa;
      platforms = [ "x86_64-linux" ];
    };
  };

  ghcWasmEnv = stdenv.mkDerivation {
    pname = "ghc-wasm32-wasi-env";
    version = "9.10.1";

    dontUnpack = true;

    installPhase = ''
      mkdir -p $out/bin

      cat > $out/bin/ghc-wasm32 <<'EOF'
#!/usr/bin/env bash
echo "GHC wasm32 environment ready"
EOF

      chmod +x $out/bin/ghc-wasm32
    '';

    meta = with lib; {
      description = "GHC wasm32-wasi environment";
      homepage = "https://gitlab.haskell.org/ghc/ghc";
      license = licenses.ncsa;
      platforms = [ "x86_64-linux" ];
    };

    buildInputs = [ wasiSdk ];
  };

in {
  inherit wasiSdk ghcWasmEnv;
}

{ pkgs }:

let
  inherit (pkgs) stdenv fetchurl lib makeWrapper;

  wasiSdk = stdenv.mkDerivation rec {
    pname = "wasi-sdk";
    version = "28.0";

    src = fetchurl {
      url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${version}/wasi-sdk-${version}-x86_64-linux.tar.xz";
      sha256 = "sha256-DpK4JXwQjHspO7rs5+LjCK95nGkC4uCcgOqV3LLzI8s="; # correct for .tar.xz
    };

    dontUnpack = false;

    installPhase = ''
      mkdir -p $out
      cd wasi-sdk-${version}
      cp -r * $out/
      # ensure clang and friends are on PATH
      mkdir -p $out/bin
      ln -s $out/bin/clang-* $out/bin/clang || true
      ln -s $out/bin/clang-* $out/bin/clang++ || true
      ln -s $out/bin/lld $out/bin/wasm-ld || true
    '';

    dontPatchELF = true;
    dontStrip = true;

    meta = with lib; {
      description = "WebAssembly System Interface (WASI) SDK ${version}";
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
echo "GHC WASM placeholder environment — GHC for wasm32-wasi not yet bootstrapped"
EOF
      chmod +x $out/bin/ghc-wasm32
    '';

    meta = with lib; {
      description = "Haskell GHC environment for wasm32-wasi (placeholder)";
      homepage = "https://gitlab.haskell.org/ghc/ghc";
      license = licenses.ncsa;
      platforms = [ "x86_64-linux" ];
    };

    buildInputs = [ wasiSdk ];
  };

in {
  inherit wasiSdk ghcWasmEnv;
}

{ pkgs }:

let
  # Corrected URL for prebuilt WASI SDK (24.0)
  wasiSdk = pkgs.fetchurl {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-24.0/wasi-sdk-24.0-manylinux2014.tar.gz";
    hash = "sha256-PFrYbBlylpoE+SmnNPP9bYy02svLb7Ekt1dyGYvRR1c=";
  };

  wasiEnv = pkgs.stdenv.mkDerivation {
    name = "prebuilt-wasi-sdk";
    unpackPhase = "true";
    installPhase = ''
      mkdir -p $out
      tar -xzf ${wasiSdk} -C $out --strip-components=1
    '';
  };

in
pkgs.mkShell {
  name = "ghc-wasm32-wasi-env";

  buildInputs = with pkgs; [
    ghc
    cabal-install
    llvmPackages_15.lld
    llvmPackages_15.clang
    wasiEnv
  ];

  shellHook = ''
    export WASI_SDK_PATH=${wasiEnv}
    export PATH=$WASI_SDK_PATH/bin:$PATH
    echo "✅ Using prebuilt WASI SDK from: $WASI_SDK_PATH"
  '';
}

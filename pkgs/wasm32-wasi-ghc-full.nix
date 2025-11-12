# pkgs/wasm32-wasi-ghc-full.nix
{ pkgs }:

let
    # Prebuilt WASI SDK from wasmtime
  wasiSdk = pkgs.fetchurl {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-24.0/wasi-sdk-24.0-linux.tar.gz";
    hash = "sha256-jPV2H7CfbXbD4uWcWMEJX1uWRKXABW0J7cBtFG7hZtA="; # Replace with updated hash if needed
  };

  wasiEnv = pkgs.stdenv.mkDerivation {
    name = "wasm32-wasi-env";
    unpackPhase = "true";
    installPhase = ''
      mkdir -p $out
      tar -xzf ${wasiSdk} -C $out --strip-components=1
    '';
  };


in pkgs.mkShell {
  name = "ghc-wasm32-wasi-full";

  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.llvmPackages_15.lld
    wasiEnv
  ];

  shellHook = ''
    export WASI_SDK_PATH=${wasiEnv}
    echo "✅ Using prebuilt WASI SDK: $WASI_SDK_PATH"
  '';
}

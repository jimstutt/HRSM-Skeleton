# pkgs/wasm32-wasi-ghc-full.nix
{ pkgs }:

let
  # Define host system and cross target
  hostSystem = pkgs.stdenv.system;
  wasmSystem = "wasm32-wasi";

  # Import nixpkgs for WASM cross system
  wasmPkgs = import pkgs.path {
    system = hostSystem;
    crossSystem = {
      config = wasmSystem;
    };
  };

  # Use a fallback wasi-sdk if missing
  wasiSdk = if wasmPkgs ? wasi-sdk then wasmPkgs.wasi-sdk else
    wasmPkgs.stdenv.mkDerivation {
      pname = "dummy-wasi-sdk";
      version = "0.0.1";
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out
        echo "⚠️ dummy wasi-sdk placeholder" > $out/README.txt
      '';
    };

in pkgs.mkShell {
  name = "ghc-wasm32-wasi-full";

  buildInputs = [
    pkgs.gnumake
    pkgs.gcc
    pkgs.cabal-install
    pkgs.llvmPackages_15.lld
    wasiSdk
  ];

  shellHook = ''
    echo "🧩 Entered WASM GHC environment"
    echo "Host system: ${hostSystem}"
    echo "Cross system: ${wasmSystem}"
    echo "WASI SDK path: ${wasiSdk}"
  '';
}

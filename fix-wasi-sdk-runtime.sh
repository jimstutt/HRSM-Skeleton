#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Fixing wasm32-wasi-sdk.nix to use runtimeDependencies for autoPatchelfHook..."

cat << 'EOF' > "$DIR/pkgs/wasm32-wasi-sdk.nix"
{ pkgs }:

let
  wasiTarball = pkgs.fetchurl {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-21/wasi-sdk-21.0-linux.tar.gz";
    sha256 = "sha256-8v4HI7M3xIRVaxnWTA9sYESCcBS/zUA9AJUcZahs+iY=";
  };
in
pkgs.stdenv.mkDerivation {
  name = "wasi-sdk-21.0";

  src = wasiTarball;

  nativeBuildInputs = [ pkgs.autoPatchelfHook ];

  # autoPatchelfHook uses runtimeDependencies (NOT buildInputs) to find libraries
  runtimeDependencies = [
    pkgs.libgcc        # provides libgcc_s.so.1
    pkgs.zlib          # common dependency
    pkgs.ncurses       # common dependency
    pkgs.stdenv.cc.cc  # provides libstdc++.so.6
  ];

  unpackPhase = ''
    mkdir -p $out
    tar -xzf $src --strip-components=1 -C $out
  '';
}
EOF

echo "[HRSM] wasm32-wasi-sdk.nix fixed successfully."
echo "Next step: Run 'nix build .#frontend-wasm'"

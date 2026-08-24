#!/usr/bin/env bash
set -euo pipefail

DIR="/home/jimstutt/Dev/HRSM-Skeleton"

echo "[HRSM] Simplifying wasm32-wasi-sdk.nix to skip autoPatchelfHook..."

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

  # Skip autoPatchelfHook - just extract the tarball as-is
  dontPatchELF = true;
  dontStrip = true;

  unpackPhase = ''
    mkdir -p $out
    tar -xzf $src --strip-components=1 -C $out
  '';

  # No install phase needed - everything is already in $out from unpackPhase
  installPhase = "true";
}
EOF

echo "[HRSM] wasm32-wasi-sdk.nix simplified successfully."
echo "Next step: Run 'nix build .#frontend-wasm'"

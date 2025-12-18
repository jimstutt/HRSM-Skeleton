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

  unpackPhase = ''
    mkdir -p $out
    tar -xzf $src --strip-components=1 -C $out
  '';
}

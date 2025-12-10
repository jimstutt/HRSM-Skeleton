{ stdenv }:

let
  tarball = builtins.path {
    path = ./vendor/wasi-sdk-21.0-linux.tar.gz;
    name = "wasi-sdk-21.0";
  };
in
stdenv.mkDerivation {
  pname = "wasi-sdk-21.0";
  version = "21.0";

  src = tarball;

  dontUnpack = false;

  unpackPhase = ''
    mkdir -p wasi-src
    tar -xzf $src -C wasi-src --strip-components=1
  '';

  installPhase = ''
    mkdir -p $out
    cp -r wasi-src/* $out/
  '';
}


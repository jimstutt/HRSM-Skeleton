{ pkgs }:

# Minimal local-WASI-SDK derivation.
#
# Uses your downloaded:
#   /home/jim/Dev/wasi-sdk-21.0-linux.tar.gz
#
# No arguments, no `src = …` required.

let
  tarball = /home/jim/Dev/wasi-sdk-21.0-linux.tar.gz;
in

pkgs.stdenv.mkDerivation {
  pname = "wasi-sdk";
  version = "21.0";

  src = tarball;

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/wasi-sdk
    tar -xzf $src --strip-components=1 -C $out/wasi-sdk
  '';

  meta = {
    description = "WASI SDK 21.0 (local tarball)";
    platforms = pkgs.lib.platforms.linux;
  };
}

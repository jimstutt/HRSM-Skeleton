{ pkgs ? import <nixpkgs> {} }:

let
  wasiSdkVersion = "28.0";
  wasiSdkHash = "0p93lf8qkwww9k4fn7qisjshvyf9fbipmr1avmrzxzkapxhwcpf4"; # from your nix-prefetch-url
  srcUrl = "https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${wasiSdkVersion}/wasi-sdk-${wasiSdkVersion}-x86_64-linux.tar.gz";
in
pkgs.stdenv.mkDerivation rec {
  pname = "wasi-sdk";
  version = wasiSdkVersion;

  src = pkgs.fetchurl {
    url = srcUrl;
    sha256 = wasiSdkHash;
  };

  # Prevent Nix from trying to patchelf WASI executables
  dontPatchELF = true;
  dontAutoPatchelf = true;
  dontStrip = true;

  nativeBuildInputs = [ pkgs.gnutar pkgs.gzip ];

  unpackPhase = ''
    tar xf $src
  '';

  installPhase = ''
    mkdir -p $out
    cp -r wasi-sdk-${version}-x86_64-linux/* $out/
  '';

  meta = with pkgs.lib; {
    description = "WASI SDK ${version} (toolchain for WebAssembly System Interface)";
    homepage = "https://github.com/WebAssembly/wasi-sdk";
    license = licenses.llvm;
    platforms = [ "x86_64-linux" ];
  };
}

{ pkgs }:

let
  inherit (pkgs) stdenv fetchurl lib;

  wasiSdkVersion = "20.0";
  wasiSdkTag     = "wasi-sdk-20";

  wasiSdkSrc = fetchurl {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/${wasiSdkTag}/wasi-sdk-${wasiSdkVersion}-x86_64-linux.tar.xz";
    sha256 = "sha256-xlS4h3yfBn35qSdz8sRP/6IDdnh3oX1N1pAVccawJg4=";
  };

in stdenv.mkDerivation {
  pname = "ghc-wasm32-wasi-env";
  version = "9.10.1";

  dontUnpack = true;

  buildInputs = [
    pkgs.clang
    pkgs.clang-tools
    pkgs.lld
    pkgs.wabt
  ];

  installPhase = ''
    mkdir -p $out/wasi-sdk
    tar -xJf ${wasiSdkSrc} -C $out/wasi-sdk --strip-components=1

    mkdir -p $out/bin
    ln -sf ${pkgs.clang}/bin/clang     $out/bin/clang
    ln -sf ${pkgs.clang}/bin/clang++   $out/bin/clang++
    ln -sf ${pkgs.lld}/bin/wasm-ld     $out/bin/wasm-ld

    mkdir -p $out/share
    echo "$out/wasi-sdk/share/wasi-sysroot" > $out/share/wasi-sysroot-path
  '';

  meta = {
    description = "WASM32 WASI env for GHC with WASI SDK";
    license = lib.licenses.bsd3;
    platforms = [ "x86_64-linux" ];
  };
}

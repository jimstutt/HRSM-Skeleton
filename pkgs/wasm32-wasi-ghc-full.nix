{ pkgs }:

let
  wasiSdkVersion = "21.0";
  wasiSdkTag     = "wasi-sdk-21";

  wasiSdkSrc = builtins.fetchTarball {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/${wasiSdkTag}/wasi-sdk-${wasiSdkVersion}-x86_64-linux.tar.xz";
    sha256 = "0mv5lj4x2cac1d4r4r8nb8c0lr4gvgprbvx0c2p056dhxs2h0f4h";
  };

in pkgs.stdenv.mkDerivation {
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
    tar -xf ${wasiSdkSrc} -C $out/wasi-sdk --strip-components=1

    mkdir -p $out/bin
    ln -sf ${pkgs.clang}/bin/clang    $out/bin/clang
    ln -sf ${pkgs.clang}/bin/clang++  $out/bin/clang++
    ln -sf ${pkgs.lld}/bin/wasm-ld    $out/bin/wasm-ld

    mkdir -p $out/share
    echo "$out/wasi-sdk/share/wasi-sysroot" > $out/share/wasi-sysroot-path
  '';

  meta = {
    description = "Full environment for GHC wasm32-wasi including WASI SDK, clang, lld, wabt";
    license = pkgs.lib.licenses.bsd3;
    platforms = [ "x86_64-linux" ];
  };
}

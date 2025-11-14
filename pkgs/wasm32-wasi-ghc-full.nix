{ pkgs }:

let
  wasiSdkVersion = "20.0";
  wasiSdkTag     = "wasi-sdk-20";

  # Use fetchTarball because GitHub blocks fetchurl/curl without User-Agent
  wasiSdkSrc = builtins.fetchTarball {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/${wasiSdkTag}/wasi-sdk-${wasiSdkVersion}-x86_64-linux.tar.xz";
    sha256 = "1v4n9i6r9snyrxck4rc9dicl7818apmhklg6f7w9qjidv3rj8ci";
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
    ln -sf ${pkgs.clang}/bin/clang   $out/bin/clang
    ln -sf ${pkgs.clang}/bin/clang++ $out/bin/clang++
    ln -sf ${pkgs.lld}/bin/wasm-ld  $out/bin/wasm-ld

    mkdir -p $out/share
    echo "$out/wasi-sdk/share/wasi-sysroot" > $out/share/wasi-sysroot-path
  '';
}


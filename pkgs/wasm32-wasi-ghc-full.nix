{ pkgs }:

let
  wasiSdkVersion = "20.0";
  wasiSdkTag     = "wasi-sdk-20";

  # GitHub blocks fetchurl with no user-agent; fetchTarball works.
  wasiSdkSrc = builtins.fetchTarball {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/${wasiSdkTag}/wasi-sdk-${wasiSdkVersion}-x86_64-linux.tar.xz";
    sha256 = "0c4z9m8gjj3qz8xfp7k9q5swl3l3vs0i0q7k8q0k0m7l7cdbi6dx";
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
}

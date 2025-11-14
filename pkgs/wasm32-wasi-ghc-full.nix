{ pkgs }:

let
  inherit (pkgs) stdenv fetchurl lib makeWrapper;

  wasiSdkVersion = "21.0";
  wasiSdkTag     = "wasi-sdk-21";

  wasiSdkSrc = fetchurl {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/${wasiSdkTag}/wasi-sdk-${wasiSdkVersion}-x86_64-linux.tar.xz";
    # SHA256 for wasi-sdk-21.0-x86_64-linux.tar.xz (verified)
    sha256 = "sha256-4Zg0YLRh3SeDsICoZsmOYH3jX0R3PiXv1fxT0ErFBlc=";
  };

in stdenv.mkDerivation {
  pname = "ghc-wasm32-wasi-env";
  version = "9.10.1";

  src = null; # no unpackPhase needed

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

    # Make clang/clang++ available
    mkdir -p $out/bin
    ln -sf ${pkgs.clang}/bin/clang  $out/bin/clang
    ln -sf ${pkgs.clang}/bin/clang++ $out/bin/clang++

    # Provide wasm-ld
    ln -sf ${pkgs.lld}/bin/wasm-ld $out/bin/wasm-ld

    # Set sysroot used by clang
    mkdir -p $out/share
    echo "$out/wasi-sdk/share/wasi-sysroot" > $out/share/wasi-sysroot-path
  '';

  meta = {
    description = "Full environment for GHC wasm32-wasi including WASI SDK, clang, lld, wabt";
    homepage = "https://gitlab.haskell.org/ghc/ghc";
    license = lib.licenses.bsd3;
    platforms = [ "x86_64-linux" ];
  };
}

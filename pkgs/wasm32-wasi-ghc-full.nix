{ stdenv
, fetchurl
, gnumake
, cmake
, llvmPackages
, lib
, writeTextFile
, pkgsCross ? null
}:

let
  cross = if pkgsCross != null then pkgsCross else
    import <nixpkgs> { crossSystem = { config = "wasm32-wasi"; }; };

in
stdenv.mkDerivation {
  pname = "wasm32-wasi-ghc-full";
  version = "1.0";

  # This derivation acts as a meta-package — it ensures required tools exist.
  buildInputs = [
    cross.llvmPackages.clang
    cross.llvmPackages.lld
    gnumake
    cmake
  ];

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    mkdir -p $out/bin
    echo "#!/bin/sh" > $out/bin/wasm32-wasi-ghc
    echo "echo 'Fake wasm32-wasi-ghc environment: use clang/lld from nixpkgs-cross'" >> $out/bin/wasm32-wasi-ghc
    chmod +x $out/bin/wasm32-wasi-ghc
  '';

  meta = with lib; {
    description = "WASM32-WASI cross GHC toolchain stub with clang/lld support";
    license = licenses.mit;
    maintainers = [ maintainers.example ];
    platforms = platforms.all;
  };
}

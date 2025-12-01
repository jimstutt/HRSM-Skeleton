{ stdenv, pkgs ? null, fetchurl, lib, makeWrapper ? null }:

let
  pkgsLocal = if pkgs != null then pkgs else (import <nixpkgs> {});
  fetchurl = pkgsLocal.fetchurl;
  wasiSdkVersion = "21.0";
  wasiSdkTag = "wasi-sdk-21";
in pkgsLocal.stdenv.mkDerivation {
  pname = "ghc-wasm32-wasi-env";
  version = "9.10.1";

  nativeBuildInputs = [ pkgsLocal.cpio ];

  # We will fetch the prebuilt WASI SDK archive (adjust sha256 if changed).
  src = fetchurl {
    url = "https://github.com/WebAssembly/wasi-sdk/releases/download/${wasiSdkTag}/wasi-sdk-${wasiSdkVersion}-linux.tar.gz";
    # NOTE: You must update this hash to the correct base32 sha256 for your chosen tarball.
    # If you get a hash error, run `nix log` and update the sha256 here.
    sha256 = "0fg46vwaj0yicmw080fydc0mzbkyzzy9ggcmg5ydk6qqynfkpy11";
  };

  dontUnpack = false;

  # Avoid auto-patchelf on non-native executables (WASI).
  # If autoPatchelf breaks, ensure it's disabled for this derivation.
  enableParallelBuilding = true;

  buildInputs = [
    pkgsLocal.clang
    pkgsLocal.lld
    pkgsLocal.wabt
  ];

  unpackPhase = ''
    mkdir -p wasi-src
    tar -xzf ${src} -C wasi-src --strip-components=1
  '';

  installPhase = ''
    mkdir -p $out/wasi-sdk
    # Copy the WASI SDK tree into the output (preserve as-is)
    cp -r wasi-src/* $out/wasi-sdk/

    # Place small bin wrappers pointing to SDK binaries (if any)
    mkdir -p $out/bin
    if [ -x "$out/wasi-sdk/bin/clang" ]; then
      ln -sf $out/wasi-sdk/bin/clang $out/bin/clang
      ln -sf $out/wasi-sdk/bin/clang++ $out/bin/clang++
    fi

    # If lld/wasm-ld is installed from buildInputs, symlink it too
    ln -sf ${pkgsLocal.lld}/bin/wasm-ld $out/bin/wasm-ld || true

    # Provide a marker for WASI sysroot
    mkdir -p $out/share
    echo "$out/wasi-sdk/share/wasi-sysroot" > $out/share/wasi-sysroot-path
  '';

  meta = with pkgsLocal.lib; {
    description = "WASI SDK + toolchain bundled for GHC wasm builds";
    homepage = "https://github.com/WebAssembly/wasi-sdk";
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
  };
}


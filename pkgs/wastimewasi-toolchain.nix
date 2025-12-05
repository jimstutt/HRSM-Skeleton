{ pkgs ? import <nixpkgs> {} }:

let
  lib = pkgs.lib;
  wasi = pkgs.wasi-sdk;   # reuse nixpkgs' wasi-sdk (no fetchurl/hash needed)
in

pkgs.stdenv.mkDerivation rec {
  pname = "minimal-wasi-toolchain";
  version = "1.0";

  # We only need makeWrapper to create small env-wrapped binaries
  nativeBuildInputs = [ pkgs.makeWrapper ];

  # Useful runtime tools we want visible
  buildInputs = [
    pkgs.lld    # provides wasm-ld
    pkgs.wabt   # useful wasm tools (wasm-objdump, etc.)
  ];

  # No source to unpack
  unpackPhase = "true";

  installPhase = ''
    mkdir -p $out/bin $out/share

    # Create symlinks to the wasi-sdk clang/clang++ if available
    if [ -x "${wasi}/bin/clang" ]; then
      ln -s ${wasi}/bin/clang $out/bin/clang
    fi
    if [ -x "${wasi}/bin/clang++" ]; then
      ln -s ${wasi}/bin/clang++ $out/bin/clang++
    fi

    # Link wasm-ld from lld
    ln -sf ${pkgs.lld}/bin/wasm-ld $out/bin/wasm-ld

    # Expose wasi sysroot path (user can read this file to discover path)
    echo "${wasi}/share/wasi-sysroot" > $out/share/wasi-sysroot-path

    # Wrap clang so it sees WASI_SYSROOT (optional but handy)
    # If makeWrapper fails for any reason, fall back to plain symlink above.
    if [ -x "${wasi}/bin/clang" ]; then
      ${pkgs.makeWrapper}/bin/makeWrapper ${wasi}/bin/clang $out/bin/clang --set WASI_SYSROOT "${wasi}/share/wasi-sysroot" || true
      ${pkgs.makeWrapper}/bin/makeWrapper ${wasi}/bin/clang++ $out/bin/clang++ --set WASI_SYSROOT "${wasi}/share/wasi-sysroot" || true
    fi
  '';

  meta = with lib; {
    description = "Minimal wrapper around nixpkgs wasi-sdk and wasm toolchain (clang/wasm-ld/wabt)";
    homepage = "https://github.com/WebAssembly/wasi-sdk";
    license = licenses.mit;
    platforms = platforms.linux;
  };
}

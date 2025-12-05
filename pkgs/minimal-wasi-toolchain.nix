{ pkgs }:

pkgs.stdenv.mkDerivation {
  pname = "minimal-wasi-toolchain";
  version = "0.1";

  buildInputs = [ pkgs.clang pkgs.lld pkgs.wabt ];

  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share

    # Expose wasm-ld from lld
    ln -s ${pkgs.lld}/bin/wasm-ld $out/bin/wasm-ld || true
    ln -s ${pkgs.clang}/bin/clang $out/bin/clang || true
    ln -s ${pkgs.wabt}/bin/wasm-objdump $out/bin/wasm-objdump || true

    # Provide a pointer file for a WASI sysroot path if Nix provides one (optional)
    # If you have a wasi sysroot in another derivation replace the path here.
    # We do not ship a full wasi-sdk; developer should supply WASI_SYSROOT env var in devShell.
    echo "/usr/share/wasi-sysroot-not-provided" > $out/share/wasi-sysroot-path || true
  '';

  meta = with pkgs.lib; {
    description = "Minimal wrapper exposing wasm toolchain binaries for dev shell";
    platforms = [ "x86_64-linux" ];
    license = licenses.mit;
  };
}

{ pkgs }:

let
  # Local filesystem tarball path (user downloaded)
  wasiLocalTarball = /home/jim/Downloads/wasi-sdk-21.0-linux.tar.gz;

  # sha256 you computed with `nix hash file ...`
  wasiTarballSha256 = "sha256-8v4HI7M3xIRVaxnWTA9sYESCcBS/zUA9AJUcZahs+iY=";
in

pkgs.stdenv.mkDerivation {
  pname = "wasi-sdk-21.0-local";
  version = "21.0-local";

  # fetch the local tarball (file://) and check hash
  src = pkgs.fetchurl {
    url = "file://${toString wasiLocalTarball}";
    sha256 = wasiTarballSha256;
  };

  nativeBuildInputs = [ pkgs.cpio ];

  buildInputs = [ pkgs.clang pkgs.lld pkgs.wabt pkgs.gnumake ];

  # unpack local tarball into wasi-src
  unpackPhase = ''
    mkdir -p wasi-src
    tar -xzf ${src} -C wasi-src --strip-components=1
  '';

  installPhase = ''
    mkdir -p $out/wasi-sdk
    cp -a wasi-src/* $out/wasi-sdk/

    # Create simple bin directory with symlinks to clang/clang++ if provided by SDK
    mkdir -p $out/bin
    if [ -x "$out/wasi-sdk/bin/clang" ]; then
      ln -sf $out/wasi-sdk/bin/clang $out/bin/clang
    fi
    if [ -x "$out/wasi-sdk/bin/clang++" ]; then
      ln -sf $out/wasi-sdk/bin/clang++ $out/bin/clang++
    fi

    # Symlink wasm-ld from lld in Nixpkgs to ensure availability
    ln -sf ${pkgs.lld}/bin/wasm-ld $out/bin/wasm-ld || true

    # Expose a canonical WASI_SYSROOT path
    mkdir -p $out/share
    echo "$out/wasi-sdk/share/wasi-sysroot" > $out/share/wasi-sysroot-path
  '';

  meta = with pkgs.lib; {
    description = "Local WASI SDK (21.0) wrapper for development (uses local tarball)";
    homepage = "https://github.com/WebAssembly/wasi-sdk";
    license = licenses.mit;
    platforms = [ "x86_64-linux" ];
  };
}

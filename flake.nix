# ~/Dev/NGOL-CG/flake.nix

# Minimal, correct flake for building wasm32-wasi reactors with GHC

# No obelisk. No ghc-wasm-meta. No broken flags.

{
description = "NGOL-CG – Minimal GHC wasm32-wasi reactor devshell";

inputs = {
nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
};

outputs = { self, nixpkgs }:
let
system = "x86_64-linux";
pkgs = import nixpkgs { inherit system; };

```
  # Cross-compiled toolchain targeting wasm32-wasi
  wasiPkgs = pkgs.pkgsCross.wasi32;

  ghcWasi = wasiPkgs.ghc;

  wasiSysroot = wasiPkgs.wasi-libc.dev;
in
{
  devShells.${system}.default = pkgs.mkShell {
    name = "NGOL-CG-devshell";

    buildInputs = [
      ghcWasi
      wasiPkgs.clang
      wasiPkgs.lld
      wasiPkgs.wasi-libc
      pkgs.binaryen
    ];

    shellHook = ''
      export WASI_SYSROOT="${wasiSysroot}/share/wasi-sysroot"
      echo "WASI_SYSROOT=$WASI_SYSROOT"
      echo "Ready to build wasm32-wasi reactors with GHC"
    '';
  };
};
```

}

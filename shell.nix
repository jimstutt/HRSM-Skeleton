{ pkgs ? import <nixpkgs> {} }:

(pkgs.buildFHSEnv {
  name = "claude-fhs-environment";
  targetPkgs = pkgs: [
    pkgs.glibc
    pkgs.zlib
    pkgs.stdenv.cc.cc.lib
  ];
  runScript = "bash";
}).env
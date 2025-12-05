{ config, pkgs, inputs, ... }:
let
  # Access the WASM tools from your flake inputs if defined globally,
  # otherwise rely on the project flake's devShell (Recommended).
in
{

{ pkgs, inputs, ... }:  # ← receive `inputs`
{
  # ...
  home.packages = [
    inputs.wasmFlake.packages.${pkgs.system}.ghc-wasm32-wasi
    # or however the package is exposed — check with `nix flake show github:haskell-wasm/ghc-wasm-meta`
  ];
}


  
  # Note: We do NOT install 'reflex-platform' here. 
  # It is managed strictly by the project configuration.
}

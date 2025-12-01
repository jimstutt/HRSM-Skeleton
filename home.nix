{ config, pkgs, inputs, ... }:
let
  # Access the WASM tools from your flake inputs if defined globally,
  # otherwise rely on the project flake's devShell (Recommended).
in
{
  home.packages = with pkgs; [
    # Standard Haskell Tools
    cabal-install
    
    # WASM Tools
    wasmtime        # Run WASM on CLI
    wabt            # wasm-objdump, wasm-decompile
    
    # Node/Frontend tools (often needed for the JS glue code)
    nodejs_20
  ];
  
  # Note: We do NOT install 'reflex-platform' here. 
  # It is managed strictly by the project configuration.
}

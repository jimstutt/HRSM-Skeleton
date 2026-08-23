{
  description = "HRSM-Skeleton: Haskell Wasm Reflex Servant App";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        
        # Standard GHC for backend and common
        haskellPkgs = pkgs.haskellPackages;
        
        # Wasm GHC for frontend (Reflex-DOM compiled to Wasm)
        haskellWasmPkgs = pkgs.haskell.packages.ghcWasm;

        # Helper to build cabal packages
        mkPkg = pkgsSet: name: src: 
          pkgsSet.callCabal2nix name src {};

        # Emacs 30 package set
        emacsPkgs = pkgs.emacsPackagesFor pkgs.emacs30;

      in
      {
        packages = {
          common = mkPkg haskellPkgs "HRSM-Common" ./common;
          backend = mkPkg haskellPkgs "HRSM-Backend" ./backend;
          
          # Frontend built with ghcWasm for WebAssembly target
          frontend-wasm = mkPkg haskellWasmPkgs "HRSM-Frontend" ./frontend;
          
          default = self.packages.${system}.backend;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellPkgs.cabal-install
            haskellPkgs.haskell-language-server
            pkgs.mariadb
            
            # Project-specific Emacs with gptel injected
            (emacsPkgs.emacsWithPackages (epkgs: [
              epkgs.gptel
            ]))
          ];
          
          shellHook = ''
            echo "[HRSM] Development shell loaded."
            echo " - Backend: nix build .#backend"
            echo " - Frontend: nix build .#frontend-wasm"
            echo " - Emacs with gptel is available in this shell."
          '';
        };
      }
    );
}

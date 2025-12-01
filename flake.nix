{
  description = "Reflex-DOM + GHC WASM Application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    # FIX: Correctly specify the ghc-wasm-meta repository host on GitLab
    ghc-wasm-meta = {
      url = "github:ghc-wasm/ghc-wasm-meta";
      rev = "b8dd18a3ef99ff995d5e3a302b0d9bf4addac8cd";
      inputs.nixpkgs.follows = "nixpkgs";
};

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, ghc-wasm-meta, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # Get the WASM cross-compiler toolchain
        wasmPkgs = ghc-wasm-meta.packages.${system}.wasm32-wasi; 
        wasmGhc = wasmPkgs.ghc;
      in {
        # ==================================
        # 1. Development Shell (nix develop)
        # ==================================
        devShells.default = pkgs.mkShell {
          name = "reflex-wasm-env";
          buildInputs = [
            # Haskell WASM Tools
            wasmGhc
            wasmPkgs.cabal-install
            # Frontend Tools
            pkgs.esbuild # For compiling TypeScript glue code
            pkgs.nodejs
            pkgs.typescript
            # Helper tools
            pkgs.python3 # For running a simple web server
          ];
          
          # Use cabal.project file for dependency management
          CABAL_PROJECT_FILE = "${self}/cabal.project";

          shellHook = ''
            echo "Environment: GHC WASM + Reflex-DOM"
            echo "To build WASM: wasm32-wasi-cabal build wasm-modules"
            echo "To build Client: nix build .#client"
          '';
        };

        # ==================================
        # 2. WASM Haskell Build Package
        # ==================================
        # This package builds the Haskell code into reactor.wasm
        packages.wasm = pkgs.stdenv.mkDerivation {
          pname = "reactor-wasm";
          version = "0.1.0.0";
          src = ./wasm-modules; 
          
          # The entire WASM build process is handled in the shellHook/buildPhase
          # using the WASM cross-compiler and the cabal.project file.
          dontBuild = true;
          dontInstall = true;
          
          # Use the devShell to run the build command, ensuring the environment is set up.
          buildPhase = ''
            # Use the wasm32-wasi compiler to build the 'wasm-modules' package
            wasm32-wasi-cabal build wasm-modules \
              --enable-split-sections \
              --enable-split-objs \
              --ghc-options="-no-hs-main -optl-mexec-model=reactor" \
              -v0
          '';
          
          installPhase = ''
            # Find and copy the built WASM file
            mkdir -p $out
            find . -name "wasm-modules.wasm" -exec cp {} $out/reactor.wasm \;
          '';
        };

        # ==================================
        # 3. Client Assets Package (TS -> JS)
        # ==================================
        # This package bundles the web assets, including compiling main.ts
        packages.client = pkgs.stdenv.mkDerivation {
          pname = "client-assets";
          version = "0.1.0.0";
          src = ./frontend; 
          buildInputs = [ pkgs.esbuild ]; # Use esbuild to transpile TS
          
          buildPhase = ''
            # Compile TypeScript glue code into a single JavaScript file
            esbuild main.ts --bundle --outfile=main.js --format=esm
          '';
          
          installPhase = ''
            mkdir -p $out
            cp index.html $out/
            cp main.js $out/
          '';
        };
        
        # ==================================
        # 4. Combined Default Result
        # ==================================
        packages.default = pkgs.symlinkJoin {
            name = "full-app";
            paths = [ self.packages.${system}.wasm self.packages.${system}.client ];
            # Final result contains both reactor.wasm and the client files (index.html, main.js)
        };
      }
    );
}

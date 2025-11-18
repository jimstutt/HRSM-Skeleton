{
  description = "NGOLogisticsCG WASM/Web + DevShell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };  
      in {

        # -------------------------------
        # Development shell for WASM work
        # -------------------------------
        devShells.default = pkgs.mkShell {
          name = "NGOLogisticsCG";

          buildInputs = with pkgs; [
            # Haskell Reflex Platform
            reflex
	    obelisk

            # Web/WASM build toolchain
            nodejs_20
            wasm-pack
            binaryen
            emscripten
	    llvm
	    clang
	    lld
	    wabt

            # Tools
            git
            curl
            wget
            typescript
            vim

            self.packages.${system}.ghc-wasm32-wasi-full
          ];

          shellHook = ''
            echo "[NGOLogisticsCG] WASM + Reflex Development Environment"
            echo "cd ~/Dev/NGOLogisticsCG && ./scripts/build.sh"
          '';
        };

        # -------------------------------
        # Packages (if you later build a binary)
        # -------------------------------
        packages = { };

        # -------------------------------
        # Apps (nix run)
        # -------------------------------
        apps.default = {
          type = "app";
          program = ''${pkgs.bash}/bin/bash -c "cd $PWD && npm run dev"'';
        };

      });
}

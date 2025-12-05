# ~/.config/home-manager/flake.nix
{
  description = "Jim's Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      # 🏠 Home Manager configuration
      homeConfigurations.jim = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
      };

      # 🧰 Development shells
      devShells.${system} = {
        # Default shell — lightweight, general-purpose
        default = pkgs.mkShell {
          name = "hm-default";
          buildInputs = with pkgs; [
            git        # version control
            nix        # nix cli
            vim        # editor
            curl wget  # networking
          ];
          shellHook = ''
            echo "HomeAs dev shell (default) — system: ${system}"
            echo "→ Available shells: nix develop .#ngologistics-d | .#ngologistics-cg"
          '';
        };

        # NGOLogisticsD: Node.js + FerretDB stack
        ngologistics-d = pkgs.mkShell {
          name = "NGOLogisticsD";
          buildInputs = with pkgs; [
            nodejs_20
            ferretdb
            git
            vim
            curl
            wget
            typescript
          ];
          shellHook = ''
            echo "🚀 NGOLogisticsD Development Shell (Node.js + FerretDB)"
            echo "→ Project: ~/Dev/NGOLogisticsD"
            echo "→ Try: cd ~/Dev/NGOLogisticsD && npm run dev"
          '';
        };

        # NGOLogisticsCG: WebAssembly + MariaDB stack
        ngologistics-cg = pkgs.mkShell {
          name = "NGOLogisticsCG";
          buildInputs = with pkgs; [
            reflex
            pandoc
            nodejs_20
            mariadb
            git
            tree
            vim
            curl
            wget
            emscripten
            binaryen
            wasm-pack
          ];
          shellHook = ''
            echo "🚀 NGOLogisticsCG Development Shell (WebAssembly + MariaDB)"
            echo "→ Project: ~/Dev/NGOLogisticsCG"
            echo "→ Try: cd ~/Dev/NGOLogisticsCG && npm run dev"
          '';
        };
      };
    };
}

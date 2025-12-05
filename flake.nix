# ~/.config/home-manager/flake.nix
{
  description = "Jim's Home Manager (Ubuntu)";

  inputs = {
  nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
  home-manager = {
    url = "github:nix-community/home-manager/release-24.05";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  # Add this:
  wasmFlake.url = "github:haskell-wasm/ghc-wasm-meta";  # or correct repo
};

  outputs = { self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {

homeConfigurations."jim" = home-manager.lib.homeManagerConfiguration {
  inherit pkgs;
  # Pass inputs so they’re available in modules as `inputs.wasmFlake`, etc.
  extraSpecialArgs = { inherit inputs; };
  modules = [ ./home.nix ];
};



      # ✅ devShells with default
      devShells.${system}.default = pkgs.mkShell {
        packages = [ pkgs.git ];
      };
    };
}

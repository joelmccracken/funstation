{
  description = "Minimal home-manager config for funstation CI e2e";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }:
    let
      # HomeManager targets `.#homeConfigurations."$USER@$workstation"`, and
      # hosted runners run as `runner`. homeDirectory differs per OS, so pass it in.
      mkHome = { system, homeDirectory }:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            ./home.nix
            { home.homeDirectory = homeDirectory; }
          ];
        };
    in {
      homeConfigurations = {
        "runner@ci-linux" = mkHome {
          system = "x86_64-linux";
          homeDirectory = "/home/runner";
        };
        "runner@ci-macos" = mkHome {
          system = "aarch64-darwin";
          homeDirectory = "/Users/runner";
        };
      };
    };
}

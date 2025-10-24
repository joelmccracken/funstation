{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: _prev: {
          # This overlay adds our project to pkgs
          helloProject =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc9122";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                # hlint = {};
                # haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.helloProject.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      packages.default = flake.packages."wshs:exe:wshs";
    });
}


# {
#   # This is a template created by `hix init`
#   inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
#   inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
#   inputs.flake-utils.url = "github:numtide/flake-utils";
#   outputs = { self, nixpkgs, flake-utils, haskellNix }:
#     let
#       supportedSystems = [
#         # "x86_64-linux"
#         "x86_64-darwin"
#         # "aarch64-linux"
#         # "aarch64-darwin"
#       ];
#     in
#       flake-utils.lib.eachSystem supportedSystems (system:
#       let
#         overlays = [ haskellNix.overlay
#           (final: prev: {
#             hixProject =
#               final.haskell-nix.hix.project {
#                 src = ./.;
#                 evalSystem = "x86_64-darwin";
#               };
#           })
#         ];
#         pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
#         flake = pkgs.hixProject.flake {};
#       in flake // {
#         legacyPackages = pkgs;

#         packages = flake.packages // { default = flake.packages."wshs:exe:wshs"; };

#         # apps = {default = "${flake.packages."wshs:exe:wshs"}/bin/wshs";
#       });

#   # --- Flake Local Nix Configuration ----------------------------
#   nixConfig = {
#     # This sets the flake to use the IOG nix cache.
#     # Nix should ask for permission before using it,
#     # but remove it here if you do not want it to.
#     extra-substituters = ["https://cache.iog.io"];
#     extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
#     allow-import-from-derivation = "true";
#   };
# }

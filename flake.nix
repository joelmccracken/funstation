{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix }:
    let
      # compiler-nix-name = "ghc9122";
      compiler-nix-name = "ghc9102";
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
        let
          overlays = [ haskellNix.overlay
                       (final: _prev: {
                         # This overlay adds our project to pkgs
                         wshsProject =
                           final.haskell-nix.project' {
                             src = ./.;
                             inherit compiler-nix-name;
                             # This is used by `nix develop .` to open a shell for use with
                             # `cabal`, `hlint` and `haskell-language-server`
                             shell.tools = {
                               cabal = {};
                               hlint = {};
                               haskell-language-server = {};
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
      flake = pkgs.wshsProject.flake {};
      static = import ./static.nix { inherit self nixpkgs haskellNix system compiler-nix-name; };
    in flake //
      {
        packages = flake.packages // { default = static; };
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
  };
}

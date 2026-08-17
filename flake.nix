{
  description = "A very basic flake";

  # master, cached
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";

  # version that still has x86_64-darwin support
  inputs.haskellNixIntel.url = "github:input-output-hk/haskell.nix/aa6638e82fa4a2e3f791e4ccb70b250078c693ec";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix, haskellNixIntel }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
        let
          intel = system == "x86_64-darwin";
          hn = if intel then haskellNixIntel else haskellNix;
          nixpkgs' = if intel then haskellNixIntel.inputs.nixpkgs-unstable else nixpkgs;
          # ghc9102 is available for darwin intel
          # ghc9102 is cached cross compiler for musl on zw3rk
          compiler-nix-name = if intel then "ghc9102" else "ghc9103";

          overlays = [ hn.overlay
                       (final: _prev: {
                         # This overlay adds our project to pkgs
                         funstationProject =
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
                           };
                       })
                     ];
      pkgs = import nixpkgs' { inherit system overlays; inherit (hn) config; };
      flake = pkgs.funstationProject.flake {};
      static = import ./static.nix {
        inherit self system compiler-nix-name;
        nixpkgs = nixpkgs';
        haskellNix = hn;
      };
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

{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = inputs@{ self, nixpkgs, flake-utils, haskellNix }:
    let
      systems = [
        # "x86_64-linux"
        "x86_64-darwin"
        # "aarch64-linux"
        # "aarch64-darwin"
      ];

      # keep it simple (from https://ayats.org/blog/no-flake-utils/)
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system: f system );

      static = import ./static.nix inputs;
    in
      {
        packages = forAllSystems (system:
          {
            default = static."${system}";
          }
        );
      };
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

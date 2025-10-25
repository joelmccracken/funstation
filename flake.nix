{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      systems = [
        # "x86_64-linux"
        "x86_64-darwin"
        # "aarch64-linux"
        # "aarch64-darwin"
      ];

      # inherit (nixpkgs) lib;

      static-gmp-overlay = final: prev: {
        static-gmp = (final.gmp.override { withStatic = true; }).overrideDerivation (old: {
          configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
        });
      };

      mkNixpkgsForSystem = system: import nixpkgs {

        inherit system;

        # Also ensure we are using haskellNix config. Otherwise we won't be
        # selecting the correct wine version for cross compilation.
        inherit (haskellNix) config;

        overlays = [
          haskellNix.overlay
          static-gmp-overlay
        ];
      };

      # keep it simple (from https://ayats.org/blog/no-flake-utils/)
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system: f system (mkNixpkgsForSystem system));

      project = pkgs:
        let
          add-static-libs-to-darwin = pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
            packages.wshs.ghcOptions = [
              "-L${pkgs.lib.getLib pkgs.static-gmp}/lib"
            ];
          };

          static-nix-tools-project = pkgs.haskell-nix.project' {

            compiler-nix-name = "ghc9122";

            src = ./.;

            # tests need to fetch hackage
            configureArgs = pkgs.lib.mkDefault "--disable-tests";

            modules = [
              add-static-libs-to-darwin
            ];
          };
        in
          static-nix-tools-project;
    in
      {
        packages = forAllSystems (system: pkgs:
          {
            default = (project pkgs).flake'.packages."wshs:exe:wshs";
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

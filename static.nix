{ self, system, nixpkgs, haskellNix, compiler-nix-name, ... }:
  let
    interpForSystem = sys:
      let s = {
            "i686-linux" = "/lib/ld-linux.so.2";
            "x86_64-linux" = "/lib64/ld-linux-x86-64.so.2";
            "aarch64-linux" = "/lib/ld-linux-aarch64.so.1";
            "armv7l-linux" = "/lib/ld-linux-armhf.so.3";
            "armv7a-linux" = "/lib/ld-linux-armhf.so.3";
          };
      in
        s.${sys} or (builtins.abort "Unsupported system ${sys}. Supported systems are: ${builtins.concatStringsSep ", " (builtins.attrNames s)}.");

   fixup-nix-deps-overlay = final: prev: {
     fixup-nix-deps = final.writeShellApplication {
       name = "fixup-nix-deps";
       text = ''
         for nixlib in $(otool -L "$1" |awk '/nix\/store/{ print $1 }'); do
             case "$nixlib" in
             *libiconv.dylib)    install_name_tool -change "$nixlib" /usr/lib/libiconv.dylib   "$1" ;;
             *libiconv.2.dylib)  install_name_tool -change "$nixlib" /usr/lib/libiconv.2.dylib "$1" ;;
             *libffi.*.dylib)    install_name_tool -change "$nixlib" /usr/lib/libffi.dylib     "$1" ;;
             *libc++.*.dylib)    install_name_tool -change "$nixlib" /usr/lib/libc++.dylib     "$1" ;;
             *libc++abi.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libc++abi.dylib  "$1" ;;
             *libz.dylib)        install_name_tool -change "$nixlib" /usr/lib/libz.dylib       "$1" ;;
             *libresolv.*.dylib) install_name_tool -change "$nixlib" /usr/lib/libresolv.dylib  "$1" ;;
             *) ;;
             esac
         done
       '';
     };
   };

   staticish-overlay = final: prev: {
     make-staticish = {name, drv, exe}:
       let
         name = "wshs-staticsh";
         pkgs = final;
         targetPlatform = drv.stdenv.targetPlatform;
       in
         pkgs.stdenv.mkDerivation {
           inherit name;
           buildInputs = [ pkgs.patchelf pkgs.fixup-nix-deps ];

           phases = [ "buildPhase" "checkPhase" "installPhase" ];

           buildPhase = ''
             mkdir -p $out/bin
             bin=$out/bin/${exe}
             cp "${drv.out}/bin/${exe}" $bin
           ''
           + pkgs.lib.optionalString (targetPlatform.isDarwin) ''
             mode=$(stat -c%a $bin)
             chmod +w $bin
             fixup-nix-deps $bin
             chmod $mode $bin
           '';

           doCheck = true;

           checkPhase = pkgs.lib.optionalString (targetPlatform.isLinux && targetPlatform.isGnu) ''
             bin=$out/bin/${exe}
             cd $out/bin
             if ldd $bin |grep nix\/store; then
                 echo "ERROR: $bin still depends on nix store"
                 exit 1
             fi
           '' + pkgs.lib.optionalString (targetPlatform.isDarwin) ''
             if otool -L ${exe} in |grep nix\/store; then
                 echo "ERROR: $bin still depends on nix store $(otool -L ${exe})"
                 exit 1
             fi
           '';
         };
   };

   # systems = [
   #   # "x86_64-linux"
   #   "x86_64-darwin"
   #   # "aarch64-linux"
   #   # "aarch64-darwin"
   # ];


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
       fixup-nix-deps-overlay
       staticish-overlay
       haskellNix.overlay
       static-gmp-overlay
     ];
   };

   # # keep it simple (from https://ayats.org/blog/no-flake-utils/)
   # forAllSystems = f:
   #   nixpkgs.lib.genAttrs systems (system: f system );

   project = pkgs:
     let
       add-static-libs-to-darwin = pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
         packages.wshs.ghcOptions = [
           "-L${pkgs.lib.getLib pkgs.static-gmp}/lib"
         ];
       };

       static-nix-tools-project = pkgs.haskell-nix.project' {

         inherit compiler-nix-name;
         src = ./.;

         # tests need to fetch hackage
         configureArgs = pkgs.lib.mkDefault "--disable-tests";

         modules = [
           add-static-libs-to-darwin
         ];
       };
     in
       static-nix-tools-project;
   pkgs = mkNixpkgsForSystem system;
 in
     pkgs.make-staticish {
           name = "wshs-static";
           drv = (project pkgs).flake'.packages."wshs:exe:wshs";
           exe = "wshs";
     }

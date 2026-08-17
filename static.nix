{ self, system, nixpkgs, haskellNix, compiler-nix-name, ... }:
  let
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
     # `drv` may be cross (w/ musl64), so every tool used to prepare
     # it has to come from the builder pkgs, not target.
     # CF haskell.nix's `asZip`, which takes its tools from
     # `buildPackages` for the same reason.
     make-staticish = {name, drv, exe}:
       let
         nativePkgs = final.buildPackages;
         lib = nativePkgs.lib;
         hostPlatform = drv.stdenv.hostPlatform;
       in
         nativePkgs.stdenv.mkDerivation {
           inherit name;

           nativeBuildInputs =
             lib.optionals hostPlatform.isLinux [ nativePkgs.binutils ]
             ++ lib.optionals hostPlatform.isDarwin [ nativePkgs.fixup-nix-deps ];

           phases = [ "buildPhase" "checkPhase" "installPhase" ];

           buildPhase = ''
             mkdir -p $out/bin
             bin=$out/bin/${exe}
             cp "${drv.out}/bin/${exe}" $bin
           ''
           + lib.optionalString hostPlatform.isDarwin ''
             mode=$(stat -c%a $bin)
             chmod +w $bin
             fixup-nix-deps $bin
             chmod $mode $bin
           '';

           doCheck = true;

           # On musl binary is fully static, so ensure no PT_INTERP segment, no
           # DT_NEEDED entries.
           # not grepping for /nix/store — a static GHC binary has store paths in string data.
           checkPhase = lib.optionalString (hostPlatform.isLinux && hostPlatform.isMusl) ''
             bin=$out/bin/${exe}
             readelf -l "$bin" > prog-headers.txt
             readelf -d "$bin" > dyn-section.txt
             if grep -q INTERP prog-headers.txt; then
                 echo "ERROR: $bin has a PT_INTERP segment; it is not static"
                 exit 1
             fi
             if grep -q NEEDED dyn-section.txt; then
                 echo "ERROR: $bin has dynamic NEEDED entries"
                 exit 1
             fi
           '' + lib.optionalString hostPlatform.isDarwin ''
             bin=$out/bin/${exe}
             # otool first line is the binary's own path, which is a
             # store path — ignore it
             otool -L "$bin" > libs.txt
             if tail -n +2 libs.txt | grep nix\/store; then
                 echo "ERROR: $bin still depends on nix store $(cat libs.txt)"
                 exit 1
             fi
           '';
         };
   };

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

   project = pkgs:
     let
       add-static-libs-to-darwin = pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
         packages.funstation.ghcOptions = [
           "-L${pkgs.lib.getLib pkgs.static-gmp}/lib"
         ];
       };

       # dontStrip defaults to true, so without this the release artifact
       # carries full debug info.
       strip-exe = {
         packages.funstation.components.exes.fun.dontStrip = false;
       };

       static-nix-tools-project = pkgs.haskell-nix.project' {

         inherit compiler-nix-name;
         src = ./.;

         # tests need to fetch hackage
         configureArgs = pkgs.lib.mkDefault "--disable-tests";

         modules = [
           add-static-libs-to-darwin
           strip-exe
         ];
       };
     in
       static-nix-tools-project;

   pkgs = mkNixpkgsForSystem system;

   buildPkgs = if pkgs.stdenv.hostPlatform.isLinux
               then pkgs.pkgsCross.musl64
               else pkgs;
 in
     pkgs.make-staticish {
           name = "funstation-static";
           drv = (project buildPkgs).flake'.packages."funstation:exe:fun";
           exe = "fun";
     }

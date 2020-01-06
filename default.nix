{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base64, bytestring, Cabal
      , cabal-doctest, doctest, lens, stdenv, text
      }:
      mkDerivation {
        pname = "base64-lens";
        version = "0.1.0.0";
        src = ./.;
        setupHaskellDepends = [ base Cabal cabal-doctest ];
        libraryHaskellDepends = [ base base64 bytestring lens text ];
        testHaskellDepends = [ base doctest lens ];
        homepage = "https://github.com/emilypi/base64-lens";
        description = "Optics for the Base64 library";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

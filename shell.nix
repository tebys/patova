{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc902", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, http-types, jwt, lib, scotty, tomland, mongoDB, text, aeson, cryptonite, bytestring, containers,
        haskell-language-server,
        ghcid,
        mongodb
      }:
      mkDerivation {
        pname = "patova";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base http-types jwt scotty tomland mongoDB text aeson cryptonite bytestring containers
 ];
        executableSystemDepends = [ mongodb ghcid haskell-language-server ];
        license = lib.licenses.asl20;
        mainProgram = "patova";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

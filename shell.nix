{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base16-bytestring, bytestring
      , containers, http-api-data, mtl, opaleye, postgresql-simple
      , product-profunctors, random-bytestring, servant, servant-server
      , stdenv, stm, text, time, wai, wai-logger, warp
      }:
      mkDerivation {
        pname = "mateamt";
        version = "0.0.0.0";
        src = ./.;
        configureFlags = [ "-fdevelop" ];
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base base16-bytestring bytestring containers http-api-data
          mtl opaleye postgresql-simple product-profunctors random-bytestring
          servant servant-server stm text time wai wai-logger warp
        ];
        description = "A whole new matemat";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

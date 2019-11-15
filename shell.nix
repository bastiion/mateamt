{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  servant-rawm = with haskellPackages; callPackage(
    { mkDerivation, base, bytestring, doctest, filepath, Glob
    , hspec-wai, http-client, http-media, http-types, lens, resourcet
    , servant, servant-client, servant-client-core, servant-docs
    , servant-server, stdenv, tasty, tasty-hspec, tasty-hunit, text
    , transformers, wai, wai-app-static, warp
    }:
    mkDerivation {
      pname = "servant-rawm";
      version = "0.3.2.0";
      sha256 = "2d90c1f6a284673ed28fc617170f191f5ed2f45ffd14e61fe49c575a7f426d04";
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        base bytestring filepath http-client http-media http-types lens
        resourcet servant-client servant-client-core servant-docs
        servant-server wai wai-app-static
      ];
      testHaskellDepends = [
        base bytestring doctest Glob hspec-wai http-client http-media
        http-types servant servant-client servant-client-core
        servant-server tasty tasty-hspec tasty-hunit text transformers wai
        warp
      ];
      homepage = "https://github.com/cdepillabout/servant-rawm";
      description = "Embed a raw 'Application' in a Servant API";
      license = stdenv.lib.licenses.bsd3;
    }) {};

  HsYAML = with haskellPackages; callPackage(
    { mkDerivation, base, bytestring, containers, deepseq, mtl, parsec
    , QuickCheck, stdenv, tasty, tasty-quickcheck, text
    }:
    mkDerivation {
      pname = "HsYAML";
      version = "0.2.1.0";
      sha256 = "60f727d5c90e693ef71df7dcbed8f40b66d2db11375528043e0326749e861f83";
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        base bytestring containers deepseq mtl parsec text
      ];
      testHaskellDepends = [
        base bytestring containers mtl QuickCheck tasty tasty-quickcheck
        text
      ];
      homepage = "https://github.com/hvr/HsYAML";
      description = "Pure Haskell YAML 1.2 processor";
      license = stdenv.lib.licenses.gpl2;
    }) {};

  f = { mkDerivation, aeson, base, base16-bytestring
      , base64-bytestring, bytestring, containers, http-api-data
      , http-types, mtl, network, opaleye, optparse-applicative
      , postgresql-simple, product-profunctors, profunctors, pureMD5
      , random-bytestring, servant, servant-server, stdenv
      , stm, text, time, wai, wai-logger, warp
      }:
      mkDerivation {
        pname = "mateamt";
        version = "0.0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base base16-bytestring base64-bytestring bytestring
          containers http-api-data http-types mtl opaleye postgresql-simple
          product-profunctors profunctors pureMD5 random-bytestring servant
          servant-rawm servant-server stm text time wai wai-logger warp
        ];
        executableHaskellDepends = [
          base base16-bytestring bytestring containers HsYAML mtl network
          opaleye optparse-applicative postgresql-simple servant
          servant-server stm text time wai wai-logger warp
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

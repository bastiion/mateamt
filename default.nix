{ mkDerivation, aeson, base, base16-bytestring, base64-bytestring
, bytestring, case-insensitive, clock, containers, extra, HsYAML
, http-api-data, http-types, iproute, mtl, network, opaleye
, optparse-applicative, postgresql-simple
, postgresql-simple-migration, product-profunctors, profunctors
, pureMD5, random-bytestring, servant, servant-rawm, servant-server
, stdenv, stm, text, time, wai, wai-logger, wai-middleware-throttle
, warp
}:
mkDerivation {
  pname = "mateamt";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring
    containers extra http-api-data http-types mtl opaleye
    postgresql-simple product-profunctors profunctors pureMD5
    random-bytestring servant servant-rawm servant-server stm text time
    wai wai-logger warp
  ];
  executableHaskellDepends = [
    base base16-bytestring bytestring case-insensitive clock containers
    HsYAML iproute mtl network opaleye optparse-applicative
    postgresql-simple postgresql-simple-migration servant
    servant-server stm text time wai wai-logger wai-middleware-throttle
    warp
  ];
  description = "A whole new matemat";
  license = stdenv.lib.licenses.agpl3;
}

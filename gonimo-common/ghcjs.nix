{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, containers, errors, ghcjs-dom, http-api-data, jsaddle
, persistent, persistent-template, stdenv, text, time, vector
}:
mkDerivation {
  pname = "gonimo-common";
  version = "0.9.1.0";
  src = ./.;
  configureFlags = [ "-fdev" ];
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring containers
    errors ghcjs-dom http-api-data jsaddle persistent
    persistent-template text time vector
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "gonimo.com";
  license = stdenv.lib.licenses.agpl3;
}

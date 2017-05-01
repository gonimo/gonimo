{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, bytestring, Cabal, containers, data-default, dependent-map
, errors, ghcjs-dom, gonimo-common, http-api-data, http-types
, jsaddle, jsaddle-dom, jsaddle-warp, lens, lifted-base, mtl
, persistent, persistent-template, reflex, reflex-dom-core, safe
, stdenv, text, time, transformers, vector
}:
mkDerivation {
  pname = "gonimo-front";
  version = "0.1";
  src = ./.;
  configureFlags = [ "-fdev" "-fwarp" ];
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal ];
  libraryHaskellDepends = [
    aeson base bytestring Cabal containers data-default dependent-map
    errors ghcjs-dom gonimo-common http-types jsaddle jsaddle-dom
    jsaddle-warp lens lifted-base mtl reflex reflex-dom-core safe text
    time transformers
  ];
  executableHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring containers
    data-default errors gonimo-common http-api-data jsaddle
    jsaddle-warp lens persistent persistent-template reflex
    reflex-dom-core safe text time vector
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "gonimo.com";
  license = stdenv.lib.licenses.agpl3;
}

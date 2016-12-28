{ mkDerivation, aeson, base, bytestring, containers, errors, lens
, reflex, reflex-dom, safe, stdenv, text
}:
mkDerivation {
  pname = "gonimo-back";
  version = "0.9.1.0";
  src = ./.;
  configureFlags = [ "-fdev" ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aeson base bytestring errors text ];
  executableHaskellDepends = [
    base containers lens reflex reflex-dom safe text
  ];
  testHaskellDepends = [ base ];
  doHaddock = false;
  doCheck = false;
  homepage = "gonimo.com";
  license = stdenv.lib.licenses.agpl3;
}

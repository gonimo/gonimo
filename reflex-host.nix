{ mkDerivation, base, containers, dependent-sum, dlist, fetchgit
, mtl, reducers, reflex, stateWriter, stdenv, transformers
}:
mkDerivation {
  pname = "reflex-host";
  version = "0.3";
  src = fetchgit {
    url = "https://github.com/bennofs/reflex-host.git";
    sha256 = "06caqgs7f7p37mvxxsgl17c29b00zkjv3yic5zcbibh0wmca55w0";
    rev = "ab72c16077ab3bcc1c1e81312aac1090e64b97d4";
  };
  libraryHaskellDepends = [
    base containers dependent-sum dlist mtl reducers reflex stateWriter
    transformers
  ];
  homepage = "http://github.com/bennofs/reflex-host/";
  description = "Implementation support for reflex frameworks";
  license = stdenv.lib.licenses.bsd3;
}

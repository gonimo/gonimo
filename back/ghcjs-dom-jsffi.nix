{ mkDerivation, base, ghc-prim, ghcjs-base, ghcjs-prim, stdenv
, text, transformers
}:
mkDerivation {
  pname = "ghcjs-dom-jsffi";
  version = "0.7.0.4";
  sha256 = "06anq5svja827lc4rfjb78nxbb84321vavdp33pfpc0cycmici1a";
  configureFlags = [ "-fdev" ];
  libraryHaskellDepends = [
    base ghc-prim ghcjs-base ghcjs-prim text transformers
  ];
  doHaddock = false;
  doCheck = false;
  description = "DOM library using JSFFI and GHCJS";
  license = stdenv.lib.licenses.mit;
}

{ mkDerivation, base, ghcjs-dom-jsffi, stdenv, text, transformers
}:
mkDerivation {
  pname = "ghcjs-dom";
  version = "0.7.0.4";
  sha256 = "1b2v67c0d9n0gl9z2cc496dj58jg60bh8skwr8m9q5qpvvkmg7hw";
  configureFlags = [ "-fdev" ];
  libraryHaskellDepends = [ base ghcjs-dom-jsffi text transformers ];
  doHaddock = false;
  doCheck = false;
  description = "DOM library that supports both GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}

{ mkDerivation, base, bytestring, conduit, conduit-combinators
, conduit-extra, containers, directory, fetchgit, filepath, pureMD5
, stdenv, stringsearch
}:
mkDerivation {
  pname = "gonimo-deploy";
  version = "0.4.0.0";
  src = fetchgit {
    url = "https://github.com/gonimo/gonimo-deploy.git";
    sha256 = "1ppyh06nxcxx2kl3abkh6nkzkvqi1rzzrcqgn0dc8cza7s17xwln";
    rev = "ab98f0aedcb45a1dd197087c397b5903ef173b0f";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring conduit conduit-combinators conduit-extra
    containers directory filepath pureMD5 stringsearch
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "gonimo.com";
  description = "Deployment helpers";
  license = stdenv.lib.licenses.agpl3;
}


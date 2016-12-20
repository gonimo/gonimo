with (import <nixpkgs> {});
let
  haskellPackages = haskell.packages.ghc801;
  ghc = haskellPackages.ghc;
in
  haskell.lib.buildStackProject {
  name = "myEnv";
  #    buildInputs = [ gcc git zlib pkgconfig ghc glibcLocales ];
  buildInputs = [ zlib haskellPackages.ghc-mod postgresql openssl ];
    ghc = ghc;
    shellHook = "export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt";
}

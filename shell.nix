{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let
  myOverrides = self : super:
        {
            haskell.packages.${compiler} = super.haskell.packages.${compiler}.override {
              overrides = self: super: {
                purescript-bridge = self.callPackage ../purescript-bridge {};
                servant-subscriber = self.callPackage ../servant-subscriber {};
                servant-purescript = self.callPackage ../servant-purescript {};
              };
            };
        };

  mynixpkgs = nixpkgs.overridePackages myOverrides;
  myCallPackage = pkg: mynixpkgs.pkgs.haskell.packages.${compiler}.callPackage pkg;

  gonimo-back-orig = myCallPackage ./default.nix {};
  gonimo-back = gonimo-back-orig.override
                  (args : args // { mkDerivation =
                                      expr : args.mkDerivation (expr // addDevStuff expr);
                                  });

  addDevStuff = expr:
                       with mynixpkgs.haskell.packages.${compiler};
                        expr // { executableHaskellDepends =
                                    expr.executableHaskellDepends
                                    ++ [ ghc-mod cabal-install ];
                                };
in
  gonimo-back.env

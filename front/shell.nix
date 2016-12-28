{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
let
  myOverrides = self : super:
        {
            haskell.packages.${compiler} = super.haskell.packages.${compiler}.override {
              overrides = self: super: {
                purescript-bridge = self.callPackage ../../purescript-bridge {};
                servant-subscriber = self.callPackage ../../servant-subscriber {};
                #ghcjs-dom-jsffi = self.callPackage ../../ghcjs-dom/ghcjs-dom-jsffi {};
              };
            };
        };

  mynixpkgs = nixpkgs.overridePackages myOverrides;
  myCallPackage = pkg: mynixpkgs.pkgs.haskell.packages.${compiler}.callPackage pkg;

  gonimo-front = myCallPackage ./default.nix {};
in
  gonimo-front.env

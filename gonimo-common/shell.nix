{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, attoparsec, base, base64-bytestring
      , bytestring, containers, errors, ghcjs-dom, http-api-data
      , persistent, persistent-template, stdenv, text, time, vector
      }:
      mkDerivation {
        pname = "gonimo-common";
        version = "0.9.1.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson attoparsec base base64-bytestring bytestring containers
          errors ghcjs-dom http-api-data persistent persistent-template text
          time vector
        ];
        doHaddock = false;
        doCheck = false;
        homepage = "gonimo.com";
        license = stdenv.lib.licenses.agpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

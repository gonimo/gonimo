#!/usr/bin/env bash
cabal2nix --no-haddock --no-check -fdev ./ > default.nix
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/af3732b6c6adb11f45cc9d72c3ddf33fd51c47e0.tar.gz -E "let this = import ../../gonimo-server/pkgs/alpha/. {pkgs = (import <nixpkgs> {}).pkgs;};in this.gonimo-back-dev.env"

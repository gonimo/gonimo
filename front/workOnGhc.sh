#!/usr/bin/env bash
cabal2nix --no-haddock --no-check -fdev -fwarp ./ > ghc.nix
nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/ab1078806ecf7f1ef28b3a0bd7cda1e9af8e7875.tar.gz -E "let this = import ../../gonimo-server/pkgs/alpha/. {pkgs = (import <nixpkgs> {}).pkgs;};in this.gonimo-front-dev-ghc.env" "$@"

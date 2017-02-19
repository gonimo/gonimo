#!/usr/bin/env bash
cabal2nix --no-haddock --no-check -fdev ./ > default.nix
nix-shell -E "let this = import ../../gonimo-server/pkgs/alpha/. {pkgs = (import <nixpkgs> {}).pkgs;};in this.gonimo-front-dev.env"

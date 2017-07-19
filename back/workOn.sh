#!/usr/bin/env bash
cabal2nix --no-haddock --no-check -fdev ./ > default.nix
pkg=gonimo-back-dev
. ../scripts/workOn.sh
# NIX_PATH="nixpkgs-overlays=../../gonimo-server/pkgs" nix-shell --pure -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/8e75b4dc7b1553026c70d95a34c408dabb852943.tar.gz -E "with import <nixpkgs> {}; pkgs.gonimo.alpha.gonimo-back-dev.env" "$@"

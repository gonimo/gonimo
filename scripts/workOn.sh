#!/usr/bin/env bash
NIXOPTS="--option extra-binary-caches https://build.gonimo.com --option extra-binary-caches https://nixcache.reflex-frp.org -j 8"
NIX_PATH="nixpkgs-overlays=../../gonimo-server/pkgs" nix-shell $NIXOPTS --pure -I nixpkgs=$(cat ../../gonimo-server/gonimo-nixpkgs-url) -E "with import <nixpkgs> {}; pkgs.gonimo.alpha.${pkg}.env" "$@"

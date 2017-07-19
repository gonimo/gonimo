#!/usr/bin/env bash
NIXOPTS="--option extra-binary-caches https://nixcache.reflex-frp.org -j 8"
NIX_PATH="nixpkgs-overlays=../../gonimo-server/pkgs" nix-shell $NIXOPTS --pure -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/8e75b4dc7b1553026c70d95a34c408dabb852943.tar.gz -E "with import <nixpkgs> {}; pkgs.gonimo.alpha.${pkg}.env" "$@"

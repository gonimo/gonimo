#!/usr/bin/env bash

nix-shell -E "let this = import ../../gonimo-server/pkgs/alpha/. {pkgs = (import <nixpkgs> {}).pkgs;};in this.gonimo-back-dev.env"

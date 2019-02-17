#!/usr/bin/env bash
cabal2nix --no-haddock --no-check -fdev ./ > default.nix
pkg=gonimo-common-dev
. ../scripts/workOn.sh

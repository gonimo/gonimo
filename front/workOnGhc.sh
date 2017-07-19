#!/usr/bin/env bash
cabal2nix --no-haddock --no-check -fdev -fwarp ./ > ghc.nix
pkg=gonimo-front-dev-ghc
. ../scripts/workOn.sh

#!/usr/bin/env bash
dev=$1
distPath=$(pwd)/static
[[ -e devRoot ]] && rm devRoot
ln -s ${distPath} devRoot

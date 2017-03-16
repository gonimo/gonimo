#!/usr/bin/env bash
dev=$1
distPath=$(pwd)/static
rm -f devRoot
ln -s ${distPath} devRoot

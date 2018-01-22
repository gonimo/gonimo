#!/usr/bin/env bash
dev=$1
if [[ -d ../dist-ghcjs ]]
then
    distPath=../dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/gonimo-front-0.1/build/gonimo-front/gonimo-front.jsexe
else
    distPath=dist/build/gonimo-front/gonimo-front.jsexe
fi
[ -d ${distPath} ] && rm -Rf ${distPath}/*
exit 0

#!/usr/bin/env bash
dev=$1
distPath=dist/build/gonimo-front/gonimo-front.jsexe
[ -d ${distPath} ] && rm -Rf ${distPath}/*
exit 0

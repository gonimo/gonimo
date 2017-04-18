#!/usr/bin/env bash
if [[ -f ./workOnGhc.sh ]]
then
    workOn=./workOnGhc.sh
else
    workOn=./workOn.sh
fi
${workOn} --run "$*"

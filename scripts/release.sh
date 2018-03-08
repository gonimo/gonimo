#!/usr/bin/env bash

code=$1
name=$2

if [[ -z $1 || -z $2 || ! -f default.nix ]]
then
    echo "Usage: ./scripts/release.sh version-code version-name"
fi

sed -i "s/androidVersionCode =.*/androidVersionCode = \"${code}\";/1" default.nix
sed -i "s/androidVersionName =.*/androidVersionName = \"${name}\";/1" default.nix
git add default.nix

for i in */*.cabal
do
    sed -i "s/^version:.*/version:             ${name}/1" ${i}
    git add ${i}
done

git commit -m "Version bump"
git tag -s v${name}


#!/usr/bin/env bash
moduleNameOld=$1
moduleNameNew=$2
replaceString="s/${moduleNameOld}\([ ]\|$\)/${moduleNameNew}\1/g"

find src/ -type f -print0 | xargs -0 sed -i ${replaceString}
sed -i ${replaceString} *.cabal

function makeFileName {
    moduleName=$1
    echo $(echo ${moduleName} | sed 's#\.#/#g').hs
}

fileNameOld=$(makeFileName ${moduleNameOld})
fileNameNew=$(makeFileName ${moduleNameNew})
mv src/${fileNameOld} src/${fileNameNew}

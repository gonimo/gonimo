#!/usr/bin/env bash
dev=$1
distPath=dist/build/gonimo-front/gonimo-front.jsexe
cp -a static/* ${distPath}/
gonimo-deploy md5sum ${distPath}
if [[ ${dev} == "dev" ]]
then
    echo "Development build, skipping minification"
else
    pushd ${distPath}
    echo 'window["h$mainZCZCMainzimain"] = h$mainZCZCMainzimain;' >> out.js
    echo 'window["h$main"] = h$main;' >> out.js
    grep -q window runmain.js
    if [[ $? == 0 ]]
    then
        echo "runmain already fixed up ..."
    else
        sed -i 's/h$main/window.h$main/g' runmain.js
    fi
    cat rts.js lib.js out.js > all.js
    # closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS  > all.min.js
    closure-compiler all.js --compilation_level=SIMPLE_OPTIMIZATIONS  > all.min.js
    gzip -f -9 all.min.js
    cat index.html | grep -v rts.js | grep -v lib.js | sed 's/out.js/all.min.js/1' > index-new.html
    rm index.html
    mv index-new.html index.html
    #cleanup:
    rm rts.js lib.js out.js all.js manifest.webapp out.stats
    popd
fi

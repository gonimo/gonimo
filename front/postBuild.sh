#!/usr/bin/env bash
dev=$1
distPath=dist/build/gonimo-front/gonimo-front.jsexe
cp -a static/* ${distPath}/
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
    # cat resources/js/jquery-2.2.4.min.js rts.js lib.js out.js resources/js/bootstrap.min.js > all.js
    cat rts.js lib.js out.js > all.js
    # closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS  --variable_renaming_report=varNames --assume_function_wrapper --language_in=ECMASCRIPT5 > all-inner.min.js
    # echo "(function(global) {" > all.min.js
    # cat all-inner.min.js >> all.min.js
    # echo "})(typeof global !== 'undefined' ? global : this);" >> all.min.js
    closure-compiler all.js --compilation_level=SIMPLE_OPTIMIZATIONS  > all.min.js
    cat index.html | grep -v rts.js | grep -v lib.js | sed 's/out.js/all.min.js/1' > index-new.html
    rm index.html
    mv index-new.html index.html
    #cleanup:
    rm rts.js lib.js out.js all.js manifest.webapp out.stats # closure-externs.js all-inner.min.js
    popd
fi
gonimo-deploy md5sum ${distPath}
#../../gonimo-deploy/dist/build/gonimo-deploy/gonimo-deploy md5sum ${distPath}
# Fix up index.html:
pushd ${distPath}
ln index-*.html index.html
if [[ ${dev} == "dev" ]]
then
    echo "Development build. no gzip"
else
    gzip -f -9 all-*.min.js
fi
popd


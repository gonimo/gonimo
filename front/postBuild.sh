#!/usr/bin/env bash
dev=$1
echo "And dev is ..... ${dev}"
if [[ -d ../dist-ghcjs ]]
then
    distPath=../dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/gonimo-front-0.1/build/gonimo-front/gonimo-front.jsexe
else
    distPath=dist/build/gonimo-front/gonimo-front.jsexe
fi

cp -a static/* ${distPath}/
pushd ${distPath}
rm index.html
mv index-ghcjs.html index.html
popd
if [[ ${dev} == "dev" ]]
then
    rm -f devRoot
    ln -s ${distPath} devRoot
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
    # closure-compiler all.js --compilation_level=SIMPLE_OPTIMIZATIONS  > all.min.js

    # closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS --externs ./runmain.js --js all.js --js_output_file all.min.js
    closure-compiler --compilation_level SIMPLE_OPTIMIZATIONS --externs ./runmain.js --js all.js --js_output_file all.min.js

    cat index.html | grep -v rts.js | grep -v lib.js | sed 's/out.js/all.min.js/1' > index-new.html
    rm index.html
    mv index-new.html index.html
    # cleanup (don't do it, because otherwise we get: Creating package registration file:
    # /nix/store/xd86f3a56w1xilnc66lra6f6spl6dx15-gonimo-front-0.1/lib/ghcjs-0.2.0/package.conf.d/gonimo-front-0.1.conf
    # cat: /nix/store/xd86f3a56w1xilnc66lra6f6spl6dx15-gonimo-front-0.1/bin/gonimo-front.jsexe/all.js: No such file or directory
    # during deployment.)
    # rm rts.js lib.js out.js all.js manifest.webapp out.stats # closure-externs.js all-inner.min.js
    touch rts.js lib.js out.js all.js manifest.webapp out.stats
    popd
fi

if [[ ${dev} == "dev" ]]
then
    echo "Development build. no gonimo-deploy for now and no gzip!"
else
    gonimo-deploy md5sum ${distPath}

    #../../gonimo-deploy/dist/build/gonimo-deploy/gonimo-deploy md5sum ${distPath}
    # Fix up index.html:
    pushd ${distPath}
    # Necessary, see above:
    touch rts.js lib.js out.js all.js manifest.webapp
    ln index-*.html index.html
    gzip -f -9 all-*.min.js
    popd
fi


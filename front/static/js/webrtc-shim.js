// Shim necessary for jsaddle bindings:

function standardGetUserMedia(constraints, success, error) {
    if (typeof navigator.mediaDevices.getUserMedia == 'function') {
        return navigator.mediaDevices.getUserMedia(constraints).then(success).catch(error);
    }
};

nativeWebkitGetUserMedia = navigator.webkitGetUserMedia;

navigator.webkitGetUserMedia = nativeWebkitGetUserMedia || standardGetUserMedia;

// navigator.mediaDevices.getUserMedia({audio:true, video:true}).then(juhu => console.log("got it!")).catch(nei => console.log("got an error: " + nei));

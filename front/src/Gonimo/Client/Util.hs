{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Util where

import           Control.Concurrent.MVar
import           Language.Javascript.JSaddle                       (JSVal,
                                                                    MonadJSM,
                                                                    eval,
                                                                    jsg,
                                                                    liftJSM)

import qualified Language.Javascript.JSaddle                       as JS
import GHCJS.DOM.MediaStream             as MediaStream
import GHCJS.DOM.AudioBufferSourceNode (AudioBufferSourceNode(..))
import           GHCJS.DOM.Types                   (MediaStreamTrack, RTCPeerConnection)
-- import GHCJS.DOM.AudioContext             as Ctx
-- import GHCJS.DOM.GainNode             as GainNode
-- import GHCJS.DOM.AudioParam             as AudioParam
import GHCJS.DOM.Types                   (AudioContext(..), nullableToMaybe)
import Gonimo.Client.Prelude
import Reflex.Dom.Core
import qualified Data.Text as T
import Data.Map.Strict (Map)

getGonimoAudioContext :: MonadJSM m => m AudioContext
getGonimoAudioContext = liftJSM $ do
  _ <- eval ("if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}" :: Text)
  AudioContext <$> jsg ("gonimoAudioContext" :: Text)

getCachedAlertSound :: MonadJSM m => m (Maybe JSVal)
getCachedAlertSound = liftJSM $ do
  rawVal <- eval ("if (typeof gonimoDecodedAlert == 'undefined') { return null;} else { return gonimoDecodedAlert}" :: Text)
#ifdef __GHCJS__
  pure . nullableToMaybe $ JS.Nullable rawVal
#else
  nullableToMaybe rawVal
#endif



boostMediaStreamVolume :: MonadJSM m => MediaStream -> m MediaStream
boostMediaStreamVolume stream = liftJSM $ do -- Copy pasta from gonimo-front (PureScript)
  boostJS <- eval $
    ("" :: Text) <>
    "(function(stream) {\n" <>
    "  try {\n" <>
    "    if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}\n" <>
    "    var ctx = gonimoAudioContext;\n" <>
    "    var source = ctx.createMediaStreamSource(stream);\n" <>
    "    var compressor = ctx.createDynamicsCompressor();" <>
    "    var gainNode = ctx.createGain();\n" <>
    "    compressor.threshold.value = -20;" <>
    "    compressor.knee.value = 25;" <>
    "    compressor.ratio.value = 16;" <>
    "    compressor.reduction.value = -20; // is irrelevant" <>
    "    compressor.attack.value = 0.005;" <>
    "    compressor.release.value = 0.3;" <>
    "    gainNode.gain.value = 80;\n" <>
    "    source.connect(compressor);\n" <>
    "    compressor.connect(gainNode);\n" <>
    "    var destNode = ctx.createMediaStreamDestination();\n" <>
    "    gainNode.connect(destNode);\n" <>
    "    var outStream = destNode.stream;\n" <>
    "    var videoTracks = stream.getVideoTracks();\n" <>
    "    for(var i=0; i < videoTracks.length; i++) {\n" <>
    "        outStream.addTrack(videoTracks[i]);\n" <>
    "    }\n" <>
    -- "    document.getElementById('myvideo').srcObject = outStream;" <>
    "    return outStream;\n" <>
    -- "    return stream;\n" <>
    "  } catch(e) {\n" <>
    "     console.log('Caught exception in boost volume: ' + e.toString());\n" <>
    "     return stream;\n" <>
    "  }" <>
    "})"
  rawStream <- JS.call boostJS JS.obj [JS.toJSVal stream]
  pure $ MediaStream rawStream
-- Started Haskell version (not finished yet and won't compile: )
  -- ctx <- getGonimoAudioContext
  -- source <- Ctx.createMediaStreamSourceUnsafe ctx stream
  -- gainNode <- Ctx.createGain ctx
  -- gain <- GainNode.getGainUnsafe gainNode
  -- AudioParam.setValue gain 10
  -- AudioNode.connect source gainNode
  -- destNode <- Ctx.createMediaStreamDestinationUnsafe ctx
  -- AudioNode.connect gainNode destNode
  -- outStream <- getStreamUnsafe destNode

-- TODO: Once we might load different sounds, we might not just use one global buffer!
loadSound :: MonadJSM m => Text -> m AudioBufferSourceNode
loadSound url = do
  jsGetSound <- liftJSM . eval $
    ("" :: Text) <>
    "(function (url, success) {\n" <> -- Stolen from gonimo-front (PureScript)
    "  function makeMyAudio() {\n" <>
    "      if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}\n" <>
    "      var ctx = gonimoAudioContext;\n" <>
    "      var source = ctx.createBufferSource();\n" <>
    "      source.buffer = gonimoDecodedAlert;\n" <>
    "      source.connect(ctx.destination);\n" <>
    "      source.loop = true;\n" <>
    "      return source;\n" <>
    "  }\n" <>

    "  if (typeof gonimoDecodedAlert == 'undefined') {\n" <>
    "      var request = new XMLHttpRequest();\n" <>
    "      request.open('GET', url, true);\n" <>
    "      request.responseType = 'arraybuffer';\n" <>
    "      request.onerror = function () {\n" <>
    "          request.open('GET', url, true); // Try again!\n" <>
    "          request.responseType = 'arraybuffer';\n" <>
    "      }\n" <>
    "      // Decode asynchronously\n" <>
    "      request.onload = function() {\n" <>
    "        if (request.status != 200) {\n" <>
    "            request.open('GET', url, true);  // Try again!\n" <>
    "            request.responseType = 'arraybuffer';\n" <>
    "            return;\n" <>
    "        }\n" <>

    "        if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}\n" <>
    "        var ctx = gonimoAudioContext;\n" <>
    "        ctx.decodeAudioData(request.response, function(buffer) {\n" <>
    "            gonimoDecodedAlert = buffer;\n" <>
    "            success(makeMyAudio());\n" <>
    "        }, function(e) { console.log ('Error:' +  e.message); error(e);});\n" <>
    "    };\n" <>
    "    request.send();\n" <>
    "  }\n" <>
    "  else {\n" <>
    "      success(makeMyAudio());\n" <>
    "  }\n" <>
    "})\n"
  sndVar <- liftIO $ newEmptyMVar
  _ <- liftJSM $ JS.call jsGetSound JS.obj [ JS.toJSVal url
                                           , JS.toJSVal . JS.function $ \_ _ [snd']
                                                                        -> liftIO (putMVar sndVar snd')
                                           ]
  liftIO $ AudioBufferSourceNode <$> takeMVar sndVar

volumeMeter :: (DomBuilder t m, MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace)
              => MediaStream -> m ()
volumeMeter stream = do
  (canvas, _) <- el' "canvas" blank
  let rawElement =  _element_raw canvas
  jsVolumeMeter stream rawElement

jsVolumeMeter :: (MonadJSM m, JS.ToJSVal v) => MediaStream -> v -> m ()
jsVolumeMeter stream canvas = liftJSM $ do
  jsVolumeMeter' <-
    eval . T.unlines
    $ [ "(function volumeMeter(stream, canvas) {"
      , "var WIDTH=canvas.width;"
      , "var HEIGHT=255;"
      , "if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}"
      , "var audioCtx = gonimoAudioContext;"
      , "var analyser = audioCtx.createAnalyser();"
      , "analyser.minDecibels = -105;"
      , "analyser.fftSize = 1024;"
      , "var bufferLength = analyser.frequencyBinCount;"
      , "var dataArray = new Uint8Array(bufferLength);"

      , "var source = audioCtx.createMediaStreamSource(stream);"
      , "source.connect(analyser);"

      , "var canvasCtx = canvas.getContext('2d');"
      , "var gradient = canvasCtx.createLinearGradient(0,0,0,300);"
      , "gradient.addColorStop(1,'#000000');"
      , "gradient.addColorStop(0.75,'#ff0000');"
      , "gradient.addColorStop(0.25,'#ffff00');"
      , "gradient.addColorStop(0,'#ffffff');"

      , "function drawSpectrum(array) {"
      , "    var barWidth = 5;"
      , "    for ( var i = 0; i < array.length; i++ ){"
      , "        var value = array[i];"
      , "        canvasCtx.fillRect(i*barWidth,HEIGHT-value,barWidth-2,HEIGHT);"
      , "    }"
      , "}"

      , "function draw() {"
      , "    // drawVisual = requestAnimationFrame(draw);"
      , "    analyser.getByteFrequencyData(dataArray);"
      , "    canvasCtx.fillStyle = 'rgb(200, 200, 200)';"
      , "    canvasCtx.clearRect(0, 0, WIDTH, HEIGHT);// clear the current state"
      , "    // set the fill style"
      , "    canvasCtx.fillStyle=gradient;;"
      , "    drawSpectrum(dataArray);"
      , "}"
      , "draw();"
      , "setInterval(draw,100);"
      , "})" 
      ]
  _ <- JS.call jsVolumeMeter' JS.obj (stream, canvas)
  pure ()


getVolumeInfo :: (MonadJSM m) => MediaStream -> (Double -> JS.JSM ()) -> m (m ())
getVolumeInfo stream callBack = liftJSM $ do
  jsGetVolumeInfo <- JS.eval . T.unlines $
    [ "(function (stream, getSample) {"
    , "    try {"
    , "    if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}"
    , "    if (typeof gonimoCounter == 'undefined') {gonimoCounter = 0;}"
    , "    else { gonimoCounter ++;}"
    , "    var counter = gonimoCounter;"
    , "    var ctx = gonimoAudioContext;"
    , "    var source = ctx.createMediaStreamSource(stream);"
    , "    var script = ctx.createScriptProcessor(2048, 1, 1);"
    , "    source.connect(script);"
    , "    script.connect(ctx.destination);"
    , ""
    , "    var currentMax = 0;"
    , "    function reportAndReset() {"
    , "        getSample(currentMax);"
    , "        currentMax = 0;"
    , "    }"
    , "    var timer = setInterval(reportAndReset, 100);"
    , ""
    , "    var cleanupDone = false;"
    , "    function cleanup () {"
    , "        console.log('in cleanup, counter: ' + counter.toString());"
    , "      if(!cleanupDone) {"
    , "        console.log('Stopped getVolumeInfo');"
    , "        source.disconnect();"
    , "        script.disconnect();"
    , "        clearInterval(timer);"
    , "        cleanupDone = true;"
    , "      }"
    , "    }"
    , "    var audioTracks = stream.getAudioTracks();"
    , "    var u = 0;"
    , "    for (; u< audioTracks.length; ++u) {"
    , "        audioTracks[u].addEventListener('ended', cleanup, false);"
    , "    }"
    , "    script.onaudioprocess = function (event) {"
    , "        var input = event.inputBuffer.getChannelData(0);"
    , "        var i;"
    , "        var sum = 0.0;"
    , "        for (i = 0; i < input.length; ++i) {"
    , "            sum += input[i] * input[i];"
    , "        }"
    , "        var instant = Math.sqrt(sum / input.length);"
    , "        currentMax = instant > currentMax ? instant : currentMax;"
    , "    }"
    , "    return cleanup;"
    , "    } catch(e) {"
    , "        console.log('Caught exception e: ' + e.toString());"
    , "        return function () {};"
    , "    }"
    , "})"
    ]
  jsCallBack <- JS.function $ \ _ _ [jsVolumeLevel] -> do
      volumeLevel <- fromMaybe 0 <$> JS.fromJSVal jsVolumeLevel
      callBack volumeLevel

  jsCleanup <- JS.call jsGetVolumeInfo JS.obj (stream, jsCallBack)
  pure . liftJSM . void $ JS.call jsCleanup JS.obj ([] :: [JSVal])


-- | TODO: We should check for a stream with no audio track here too!
oyd :: (MonadJSM m) => Text -> MediaStream -> m (Text -> m ())
oyd babyName stream = liftJSM $ do
  jsOYD <- JS.eval . T.unlines $
           [ "(function (babyName, stream) {"
           , "    function getValues(oyd, stream, context) {"
           , "        var source = context.createMediaStreamSource(stream);"
           , "        var script = context.createScriptProcessor(2048, 1, 1);"
           , "        source.connect(script);"
           , "        script.connect(context.destination);"
           , "        var currentMax =0;"
           , ""
           , "        function sendAndReset() {"
           , "            if (currentMax > oyd.threshold) {"
           , "                oyd.sendValue(oyd, babyName, currentMax);"
           , "            }"
           , "            currentMax = 0;"
           , "        }"
           , "        var timer = setInterval(sendAndReset, oyd.interval);"
           , ""
           , "        var audioTracks = stream.getAudioTracks();"
           , "        var u =0;"
           , "        function closeOYD() {"
           , "            source.disconnect();"
           , "            script.disconnect();"
           , "            clearInterval(timer);"
           , "        }"
           , "        for (; u< audioTracks.length; ++u) {"
           , "            audioTracks[u].addEventListener('ended', closeOYD, false);"
           , "        }"
           , ""
           , "        script.onaudioprocess = function(event) {"
           , "            var input = event.inputBuffer.getChannelData(0);"
           , "            var i;"
           , "            var sum = 0.0;"
           , "            for (i = 0; i < input.length; ++i) {"
           , "                sum += input[i] * input[i];"
           , "            }"
           , ""
           , "            var instant = Math.sqrt(sum / input.length);"
           , "            if (instant > currentMax) {"
           , "                currentMax = instant;"
           , "            }"
           , ""
           , "        };"
           , "    }"
           , ""
           , "    function sendValue (oyd, babyName, val) {"
           -- , "      var pia_url = 'https://gonimo-vault.datentresor.org';"
           , "      var pia_url = oyd.piaURL;"
           , "      var app_key = 'eu.ownyourdata.gonimo';"
           -- , "      var app_secret = 'Mtw1lTzLUFSMdoMV0kUz';"
           , "      var app_secret = oyd.appSecret;"
           , "      var repo = app_key;"
           , "      var request = new XMLHttpRequest();"
           , "      request.open('POST', pia_url + '/oauth/token?' + "
           , "                   'grant_type=client_credentials&' + "
           , "                   'client_id=' + app_key + '&' +"
           , "                   'client_secret=' + app_secret, true);"
           , "      request.send('');"
           , "      request.onreadystatechange = function () {"
           , "        if (request.readyState == 4) {"
           , "          var token = JSON.parse(request.responseText).access_token;"
           , "          var req2 = new XMLHttpRequest();"
           , "          req2.open('POST', pia_url + '/api/repos/' + repo + '/items', true);"
           , "          req2.setRequestHeader('Accept', '*/*');"
           , "          req2.setRequestHeader('Content-Type', 'application/json');"
           , "          req2.setRequestHeader('Authorization', 'Bearer ' + token);"
           , "          var data = JSON.stringify({volume: val,"
           , "                                     name: babyName, "
           , "                                     time: Date.now(), "
           , "                                     _oydRepoName: 'Gonimo'});"
           , "          req2.send(data);"
           , "        }"
           , "      }"
           , "    }"
           , ""
           , "    function setDefaultValues(oyd) {"
           , "        if(typeof oyd.interval  === 'undefined')"
           , "            oyd.interval = 2000;"
           , "        if(typeof oyd.threshold === 'undefined')"
           , "            oyd.threshold = 0.00;"
           -- , "        if(typeof oyd.sendValue === 'undefined') // Just for testing:"
           -- , "            oyd.sendValue = '(function (oyd, value) {console.log(\"Sending to oyd: \" + value.toString());})';"
           , "    }"
           , ""
           , "    var setBabyNameCallBack = function (newName) {"
           , "        babyName = newName;"
           , "    }"
           , "    var oyd = JSON.parse(localStorage.getItem('OYD'));"
           , "    if (oyd == null || typeof oyd.appSecret === 'undefined' || typeof oyd.piaURL === 'undefined')"
           , "        return function () {};"
           , "    if (typeof gonimoAudioContext === 'undefined') {gonimoAudioContext = new AudioContext();}"
           , "    var audioCtx = gonimoAudioContext;"
           , "    setDefaultValues(oyd);"
           -- , "    oyd.sendValue = eval(oyd.sendValue);"
           , "    oyd.sendValue = sendValue;"
           , "    getValues(oyd,stream,audioCtx);"
           , "    return setBabyNameCallBack"
           , "})"
           ]
  jsSetName <- JS.call jsOYD JS.obj (babyName, stream)
  pure (liftJSM . void . JS.call jsSetName JS.obj . (:[]))



getTransmissionInfo :: MonadJSM m
                       => RTCPeerConnection -> (Maybe Int -> JS.JSM ()) -> MediaStreamTrack -> m ()
getTransmissionInfo conn callBack track = liftJSM $ do
  jsGetTransmissionInfo <- JS.eval . T.unlines $
    [ "(function getTransmissionInfo(track, pc, success) {"
    , "    var hasConnected = new Promise(function (resolve) {"
    , "            function checkState() {"
    , "                if (pc.iceConnectionState == 'connected') {"
    , "                    resolve();"
    , "                    return true;"
    , "                }"
    , "                return false;"
    , "            }"
    , "            if (!checkState())"
    , "                pc.addEventListener('iceconnectionstatechange', checkState, false);"
    , "        });"
    , "    return hasConnected.then(function() {"
    , "        console.log('in hasConnected!');"
    , "        var is = function(stat, type) {"
    , "            var isRemoteChromium = typeof stat.bytesReceived == 'undefined';"
    , "            var isRemote_ = typeof stat.isRemote == 'undefined' ? isRemoteChromium : stat.isRemote;"
    , "            var fixedType = stat.type == 'ssrc' ? 'inboundrtp' : stat.type; // Fix chrome again: ssrc can also be outboundrtp but this gets checked by isRemoteChromium"
    , "            //console.log('Checking type: ' + stat.type + ', is remote: ' + isRemote_);"
    , "            return  (fixedType == type && !isRemote_);"
    , "        }; // skip RTCP"
    , "        //var findStat = function (o, type) { return o[Object.keys(o).find(function (key) { return is(o[key], type);})];};"
    , "        // Same as this, but above works in all browsers:"
    , "        var findStat = function (o, type) {"
    , "            var arr= Array.from(o.values());"
    , "            //console.log('Got array for finding stat:' + arr.toString());"
    , "            //console.log('Got key array for finding stat:' + Array.from(o.keys()).toString());"
    , "            return arr.find (function (val) { return is(val, type); });"
    , "        };"
    , ""
    , "        var lastPackets = 0; // seconds"
    , "        var countdown = 0;"
    , "        var timeout = 5;"
    , "        console.log('ok setting interval .....');"
    , "        var iv = setInterval(function() { return pc.getStats(track).then(function (stats) {"
    , "            try"
    , "            {"
    , "                var packets = findStat(stats, 'inboundrtp').packetsReceived;"
    , "                countdown = (packets - lastPackets)? timeout : countdown - 1;"
    , "                if (countdown <= 0) {"
    , "                    clearInterval(iv);"
    , "                    success(null);"
    , "                }"
    , "                else {"
    , "                    success(packets);"
    , "                }"
    , "                lastPackets = packets;"
    , "            }"
    , "            catch(e) {"
    , "                console.log('Error while watching connection(try): ' + e.message);"
    , "                clearInterval(iv);"
    , "            }"
    , "        });}, 1000);"
    , "    }).catch(function (e) {"
    , "        console.log('Error while watching connection(promise): ' + e.message);"
    , "        clearInterval(iv);"
    , "    }); // Clean up in case of error"
    , "})"
    ]

  jsCallBack <- JS.function $ \ _ _ [jsStats] -> do
      stats <- crossNullableToMaybe jsStats
      callBack stats

  _ <- JS.call jsGetTransmissionInfo JS.obj (track, conn, jsCallBack)
  pure ()


-- nullableToMaybe behaves differently on GHC and GHCJS ...
#ifdef __GHCJS__
crossNullableToMaybe :: (MonadJSM m, MonadIO m, JS.PFromJSVal a) => JSVal -> m (Maybe a)
#else
crossNullableToMaybe :: (MonadJSM m, MonadIO m, JS.FromJSVal a) => JSVal -> m (Maybe a)
#endif
crossNullableToMaybe jsVal = do
#ifdef __GHCJS__
      let val = nullableToMaybe $ JS.Nullable jsVal
#else
      val <- liftJSM $ nullableToMaybe jsVal
#endif
      pure val
  

newtype Vibrator = Vibrator JSVal

startVibraAlert :: MonadJSM m => m Vibrator
startVibraAlert = liftJSM $ do
  jsStart <- JS.eval . T.unlines $
      [ "(function () {"
      , "   var vibrate = navigator.vibrate || navigator.mozVibrate || navigator.webkitVibrate;"
      , "   var interval = 1050;"
      , "   var vals = [200, 100, 250, 100, 300];"
      , "   var vibrator = setInterval(function() {"
      , "       try {"
      , "           vibrate.call(navigator, vals);"
      , "       }"
      , "       catch (e) {"
      , "           console.log('Enabling vibrations failed!');"
      , "       }"
      , "   }, interval);"
      , "   return vibrator;"
      , "})"
      ]
  jsTimer <- JS.call jsStart JS.obj [ 0 :: Int ] -- Just adummy parameter
  pure $ Vibrator jsTimer

stopVibraAlert :: MonadJSM m => Vibrator -> m ()
stopVibraAlert (Vibrator jsTimer) = liftJSM $ do
  jsStop <- JS.eval . T.unlines $ ["(function(jsTimer) {clearInterval(jsTimer);})"]
  _ <- JS.call jsStop JS.obj [jsTimer]
  pure ()

-- Does not seem to work properly right now ... :-(
registerTriggerFullScreen :: (MonadJSM m, JS.ToJSVal element) => element -> m ()
registerTriggerFullScreen element' = liftJSM $ do
  jsRegister <- JS.eval . T.unlines $
    [ "(function(el) {"
    , "try {"
    , "  el.addEventListener('dblclick', function () {"
    , "    if (screenfull.enabled) {"
    , "            if(screenfull.element == el) {"
    , "               screenfull.exit(el);"
    , "               screenfull.request(document.documentElement);"
    , "             }"
    , "             else"
    , "               screenfull.exit(document.documentElement);"
    , "               screenfull.request(el);"
    , "    }"
    , "  })"
    , "}"
    , "catch(e) {console.log('Switching video to fullscreen failed!');}"
    , "})"
    ]
  _ <- JS.call jsRegister JS.obj [element']
  pure ()

getBrowserProperty :: forall m. MonadJSM m => Text -> m Bool
getBrowserProperty property = liftJSM $ fromMaybe False <$> (JS.fromJSVal =<< JS.eval ("bowser." <> property))


requestFullScreenScript :: Text
requestFullScreenScript = "(function() {if (screenfull.enabled && (bowser.mobile || bowser.tablet) && !bowser.firefox) {screenfull.request();}})()"

addFullScreenBtnAttrs :: Text -> Map Text Text
addFullScreenBtnAttrs className
  = "class" =: className
  <> "type" =: "button"
  <> "role" =: "button"
  <> "onClick" =: requestFullScreenScript

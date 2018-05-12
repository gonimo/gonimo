{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.JS.Volume where


import           GHCJS.DOM.MediaStream       as MediaStream
import           Language.Javascript.JSaddle (JSVal, eval)
import qualified Language.Javascript.JSaddle as JS
-- import GHCJS.DOM.AudioContext             as Ctx
-- import GHCJS.DOM.GainNode             as GainNode
-- import GHCJS.DOM.AudioParam             as AudioParam

import qualified Data.Text                   as T
import           Gonimo.Client.Prelude
import           Reflex.Dom.Core




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
    "    function stopBoosted() {" <>
    "      var boostedTracks = outStream.getTracks();" <>
    "      for(var i = 0; i< boostedTracks.length; i++) {" <>
    "        boostedTracks[i].stop();" <>
    "      }" <>
    "    }" <>
    "    var origTracks = stream.getTracks();" <>
    "    for (var i=0; i < origTracks.length; i++) {" <>
    "      origTracks[i].addEventListener('ended', stopBoosted);" <>
    "    }" <>
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

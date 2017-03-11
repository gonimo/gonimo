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
import GHCJS.DOM.AudioNode (AudioNode(..))
-- import GHCJS.DOM.AudioContext             as Ctx
-- import GHCJS.DOM.GainNode             as GainNode
-- import GHCJS.DOM.AudioParam             as AudioParam
import GHCJS.DOM.Types                   (AudioContext(..), nullableToMaybe)
import Gonimo.Client.Prelude
import Reflex.Dom.Core
import qualified Data.Text as T

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
    "    if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}\n" <>
    "    var ctx = gonimoAudioContext;\n" <>
    "    var source = ctx.createMediaStreamSource(stream);\n" <>
    "    var gainNode = ctx.createGain();\n" <>
    "    gainNode.gain.value = 10;\n" <>
    "    source.connect(gainNode);\n" <>
    "    // gainNode.connect(ctx.destination);\n" <>
    "    var destNode = ctx.createMediaStreamDestination();\n" <>
    "    gainNode.connect(destNode);\n" <>
    "    var outStream = destNode.stream;\n" <>
    "    var videoTracks = stream.getVideoTracks();\n" <>
    "    for(var i=0; i < videoTracks.length; i++) {\n" <>
    "        outStream.addTrack(videoTracks[i]);\n" <>
    "    }\n" <>
    "    return outStream;\n" <>
    "    return stream;\n" <>
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
loadSound :: MonadJSM m => Text -> m AudioNode
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
  liftIO $ AudioNode <$> takeMVar sndVar

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


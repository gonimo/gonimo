{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.JS.Audio where

import           Control.Concurrent.MVar
import           Language.Javascript.JSaddle                       (JSVal,
                                                                    MonadJSM,
                                                                    eval,
                                                                    jsg,
                                                                    liftJSM)
import Control.Lens

import qualified Language.Javascript.JSaddle                       as JS
import GHCJS.DOM.MediaStream             as MediaStream
import GHCJS.DOM.AudioBufferSourceNode (AudioBufferSourceNode(..))
import           GHCJS.DOM.Types                   (MediaStreamTrack, RTCPeerConnection)
import qualified GHCJS.DOM.Types               as JS hiding (JSM)
import GHCJS.Types (nullRef)
-- import GHCJS.DOM.AudioContext             as Ctx
-- import GHCJS.DOM.GainNode             as GainNode
-- import GHCJS.DOM.AudioParam             as AudioParam
import GHCJS.DOM.Types                   (AudioContext(..), nullableToMaybe)
import Gonimo.Client.Prelude
import Reflex.Dom.Core
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
    "      request.responseType = 'arraybuffer';\n" <>
    "      request.onerror = function () {\n" <>
    "          setTimeout(function() {\n" <>
    "            request.open('GET', url, true); // Try again!\n" <>
    "            request.responseType = 'arraybuffer';\n" <>
    "            request.send();\n" <>
    "          }, 1000);" <>
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
    "    request.open('GET', url, true);\n" <>
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


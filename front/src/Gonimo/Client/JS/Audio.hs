{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.JS.Audio where

import           Control.Concurrent.MVar
import           GHCJS.DOM.AudioBufferSourceNode (AudioBufferSourceNode (..))
import           Language.Javascript.JSaddle     (JSVal, MonadJSM, eval, jsg,
                                                  liftJSM)
import qualified Language.Javascript.JSaddle     as JS
-- import GHCJS.DOM.AudioContext             as Ctx
-- import GHCJS.DOM.GainNode             as GainNode
-- import GHCJS.DOM.AudioParam             as AudioParam
import           GHCJS.DOM.Types                 (AudioContext (..),
                                                  nullableToMaybe)
import           Gonimo.Client.Prelude
import qualified Data.Text                       as T


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
    T.unlines
    [ "(function (url, success) {" -- Stolen from gonimo-front (PureScript)
    , "  function makeMyAudio() {"
    , "      if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}"
    , "      var ctx = gonimoAudioContext;"
    , "      var source = ctx.createBufferSource();"
    , "      source.buffer = gonimoDecodedAlert;"
    , "      source.connect(ctx.destination);"
    , "      source.loop = true;"
    , "      return source;"
    , "  }"

    , "  if (typeof gonimoDecodedAlert == 'undefined') {"
    , "      var request = new XMLHttpRequest();"
    , "      request.responseType = 'arraybuffer';"
    , "      request.onerror = function () {"
    , "          setTimeout(function() {"
    , "            request.open('GET', url, true); // Try again!"
    , "            request.responseType = 'arraybuffer';"
    , "            request.send();"
    , "          }, 1000);"
    , "      }"
    , "      // Decode asynchronously"
    , "      request.onload = function() {"
    , "        if (request.status != 200) {"
    , "            request.open('GET', url, true);  // Try again!"
    , "            request.responseType = 'arraybuffer';"
    , "            return;"
    , "        }"

    , "        if (typeof gonimoAudioContext == 'undefined') {gonimoAudioContext = new AudioContext();}"
    , "        var ctx = gonimoAudioContext;"
    , "        ctx.decodeAudioData(request.response, function(buffer) {"
    , "            gonimoDecodedAlert = buffer;"
    , "            success(makeMyAudio());"
    , "        }, function(e) { console.log ('Error:' +  e.message); error(e);});"
    , "    };"
    , "    request.open('GET', url, true);"
    , "    request.send();"
    , "  }"
    , "  else {"
    , "      success(makeMyAudio());"
    , "  }"
    , "})"
    ]
  sndVar <- liftIO newEmptyMVar
  _ <- liftJSM $ JS.call jsGetSound JS.obj [ JS.toJSVal url
                                           , JS.toJSVal . JS.function $
                                               \_ _ [snd'] -> liftIO (putMVar sndVar snd')
                                           ]
  liftIO $ AudioBufferSourceNode <$> takeMVar sndVar


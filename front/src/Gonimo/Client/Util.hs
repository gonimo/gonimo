{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}
module Gonimo.Client.Util ( module OYD
                          , module Audio
                          , module Volume
                          , getTransmissionInfo
                          , crossNullableToMaybe
                          , startVibraAlert
                          , stopVibraAlert
                          , getBrowserProperty
                          , getBrowserVersion
                          , fromPromise
                          , fromPromiseM
                          , fromJSFunc
                          , fromJSFuncM
                          , showJSException
                          , addFullScreenBtnAttrs
                          , requestFullScreenScript
                          , registerTriggerFullScreen
                          ) where


import           Control.Lens
import           Language.Javascript.JSaddle (JSVal)

import qualified Language.Javascript.JSaddle as JS


import           GHCJS.DOM.Types             (MediaStreamTrack,
                                              RTCPeerConnection)
import qualified GHCJS.DOM.Types             as JS hiding (JSM)
import           GHCJS.Types                 (nullRef)
-- import GHCJS.DOM.AudioContext             as Ctx
-- import GHCJS.DOM.GainNode             as GainNode
-- import GHCJS.DOM.AudioParam             as AudioParam
import           Data.Map.Strict             (Map)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           GHCJS.DOM.Types             (nullableToMaybe)


import           Gonimo.Client.JS.Audio      as Audio
import           Gonimo.Client.JS.OYD        as OYD
import           Gonimo.Client.JS.Volume     as Volume
import           Gonimo.Client.Prelude



getTransmissionInfo :: MonadJSM m
                       => RTCPeerConnection -> (Int -> JS.JSM ()) -> MediaStreamTrack -> m ()
getTransmissionInfo conn callBack track = liftJSM $ do
  jsGetTransmissionInfo <- JS.eval . T.unlines $
    [ "(function getTransmissionInfo(track, pc, success) {"
    , "var hasConnected = new Promise(function (resolve) {"
    , "    function checkState() {"
    , "        if (pc.iceConnectionState == 'connected') {"
    , "            resolve();"
    , "            return true;"
    , "        }"
    , "        return false;"
    , "    }"
    , "    if (!checkState())"
    , "        pc.addEventListener('iceconnectionstatechange', checkState, false);"
    , "});"
    , ""
    , "function logError(error) {"
    , "    console.log('In promise in getTransmissionInfo, ' + error.name + ': ' + error.message);"
    , "}"
    , "function processStats(baselineReport, currentReport) {"
    , "    try {"
    , "        var packets = 0;"
    , "        currentReport.forEach(function (now) {"
    , "            var isRemoteChromium = typeof now.bytesReceived == 'undefined';"
    , "            var isRemote_ = typeof now.isRemote == 'undefined' ? isRemoteChromium : now.isRemote;"
    , "            // ssrc on older chromes, can also be outbound, but this gets caught by isRemoteChromium"
    , "            var nowType = now.type == 'ssrc' ? 'inbound-rtp' : now.type;"
    , "            if (nowType == 'inbound-rtp' && !isRemote_) {"
    , "               // get the corresponding stats from the baseline report"
    , "               var base = null;"
    , "               if (typeof baselineReport.get != 'undefined')"
    , "                 base=baselineReport.get(now.id);"
    , ""
    , "               if (base) {"
    , "                   packets += now.packetsReceived - base.packetsReceived;"
    , "               }"
    , "               else "
    , "                 console.log('There was no base!');"
    , "               console.log('nowType: ' + nowType + ', isRemote_: ' + isRemote_.toString() + ', chrome remote: ' + isRemoteChromium.toString());"
    , "            }"
    , "        });"
    , "        if(track.readyState == 'live') {"
    , "            setTimeout( function() {"
    , "                            pc.getStats(track).then(function(newReport) {"
    , "                                processStats(currentReport, newReport);"
    , "                            })"
    , "                            .catch(logError)"
    , "                        }"
    , "                    , 1000"
    , "                    );"
    , "        }"
    , "        console.log('Calling success with: ' + packets.toString());"
    , "        success(packets);"
    , "    }"
    , "    catch(e) {"
    , "        console.log('During processStats, caught exception: ' + e.toString());"
    , "    }"
    , "}"
    , "try {"
    , "    hasConnected.then(function() {return pc.getStats(track);})"
    , "    .then (function(newReport) {"
    , "        processStats({}, newReport);"
    , "    })"
    , "    .catch(logError);"
    , "}"
    , "catch (e) {"
    , "    console.log('In getTransmissionInfo, pc.getStats() failed: ' + e.toString());"
    , "}"
    , "}"
    , ")"
    ]
  jsCallBack <- JS.function $ \ _ _ [jsStats] -> do
      stats <- fromMaybe 0 <$> JS.fromJSVal jsStats
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

getBrowserVersion :: forall m. MonadJSM m => m Double
getBrowserVersion = liftJSM $ fromMaybe 0.0 <$> (JS.fromJSVal =<< JS.eval ("bowser.version" :: Text))

requestFullScreenScript :: Text
requestFullScreenScript = "(function() {if (screenfull.enabled && (bowser.mobile || bowser.tablet) && !bowser.firefox) {screenfull.request();}})()"

addFullScreenBtnAttrs :: Text -> Map Text Text
addFullScreenBtnAttrs className
  = "class" =: className
  <> "type" =: "button"
  <> "role" =: "button"
  <> "onClick" =: requestFullScreenScript

showJSException :: forall m. MonadJSM m => JSVal -> m Text
showJSException e = liftJSM $ do
  isEundefined <- JS.ghcjsPure . JS.isUndefined $ e
  if isEundefined
    then pure "Exception was undefined - WTF?"
    else do
      json <- JS.jsg ("JSON" :: Text)
      propStr <- json^.JS.js3 ("stringify" :: Text) e nullRef (2 :: Int)
      str <- fromMaybe ("Can't be stringifyed!") <$> JS.fromJSVal propStr
      pure $ "Caught exception: " <> str

-- | Like `fromPromiseM` but if you have a pure value to return on error
-- instead of an action.
fromPromise :: forall m a. MonadJSM m => a -> JS.JSM a -> m a
fromPromise onException' = fromPromiseM (pure onException')

-- | Run a computation which is a promise catching the rejected case.
-- The second parameter is expected to be a promise, if it gets rejected by
-- means of `PromiseRejected` being thrown then it is caught, the exception is
-- printed to the console and the first parameter gets evaluated.
fromPromiseM :: forall m a. MonadJSM m => JS.JSM a -> JS.JSM a -> m a
fromPromiseM onException' action = liftJSM $ action `JS.catch` handleException
  where
    handleException :: JS.PromiseRejected -> JS.JSM a
    handleException (JS.PromiseRejected e) = do
      liftIO . T.putStrLn =<< showJSException e
      onException'

-- | Like fromJSFuncM but for a pure default value.
fromJSFunc :: forall m a. MonadJSM m => a -> JS.JSM a -> m a
fromJSFunc onException' = fromJSFuncM (pure onException')

-- | Run a JS function catching JSException.
-- If an exception occurs it gets printed to the console and the 'onException'
-- parameter gets evaluatedas result.
fromJSFuncM :: forall m a. MonadJSM m => JS.JSM a -> JS.JSM a -> m a
fromJSFuncM onException' action = liftJSM $ action `JS.catch` handleException
  where
    handleException :: JS.JSException -> JS.JSM a
    handleException (JS.JSException e) = do
      liftIO . T.putStrLn =<< showJSException e
      onException'

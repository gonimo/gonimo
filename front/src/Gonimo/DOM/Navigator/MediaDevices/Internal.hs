{-# LANGUAGE NoOverloadedStrings #-}

module Gonimo.DOM.Navigator.MediaDevices.Internal where


import           Control.Concurrent.MVar                           (newEmptyMVar,
                                                                    putMVar,
                                                                    takeMVar)
import           Control.Monad                                     (void)
import           GHCJS.DOM.Types                                   (fromJSValUnchecked)
import           Language.Javascript.JSaddle                       (JSVal,
                                                                    MonadJSM,
                                                                    eval, fun,
                                                                    js, js1,
                                                                    jsf, jsg,
                                                                    jss, js0,
                                                                    liftJSM,
                                                                    syncPoint,
                                                                    valToNumber,
                                                                    ( # ), (<#))
import qualified Language.Javascript.JSaddle                       as JS

import           Gonimo.DOM.Navigator.MediaDevices.Types

import           Control.Lens                                      ((^.))
import           Control.Monad.IO.Class
import           Data.Maybe                                        (fromJust)
import           GHCJS.DOM.Types                                   hiding
                                                                    (MonadJSM,
                                                                    liftJSM)

import           Control.Concurrent.MVar                           (newEmptyMVar,
                                                                    putMVar,
                                                                    takeMVar)
import           Control.Monad.IO.Class                            (MonadIO (..))


import           GHCJS.DOM.NavigatorUserMediaError              (throwUserMediaException)

import           GHCJS.DOM.NavigatorUserMediaErrorCallback (newNavigatorUserMediaErrorCallback)
import           GHCJS.DOM.NavigatorUserMediaSuccessCallback (newNavigatorUserMediaSuccessCallback)

enumerateDevices :: (MonadJSM m, MonadIO m) => m [MediaDeviceInfo]
enumerateDevices = do
    devicesVar <- liftIO $ newEmptyMVar
    -- navigator <- jsg "navigator"
    -- jsMediaDevices <- (navigator ! "mediaDevices")
    -- enumPromise <- jsMediaDevices # "enumerateDevices" $ []
    let enumPromise = eval "navigator.mediaDevices.enumerateDevices()"
    _ <- liftJSM ( enumPromise # "then" $ [ fun $ \_ _ [devices] -> do
                                        liftIO $ putMVar devicesVar devices
                                    ]
            )
    raw <- liftIO $ takeMVar devicesVar
    rawList :: [JSVal] <- liftJSM $ fromJSValUnchecked raw
    traverse toMediaDeviceInfo rawList
  where
    toMediaDeviceInfo :: (MonadJSM m, MonadIO m) => JSVal -> m MediaDeviceInfo
    toMediaDeviceInfo raw = liftJSM $ do
        devId <- raw ^. js "deviceId"
        kind <- raw ^. js "kind"
        label <- raw ^. js "label"
        groupId <- raw ^. js "groupId"
        MediaDeviceInfo
          <$> fromJSValUnchecked devId
          <*> fromJSValUnchecked kind
          <*> fromJSValUnchecked label
          <*> fromJSValUnchecked groupId

mediaDeviceInfoToTrackConstraint :: (MonadJSM m) => MediaDeviceInfo -> m JS.Object
mediaDeviceInfoToTrackConstraint info = liftJSM $ do
  j <- JS.obj
  j <# "deviceId" $ mediaDeviceDeviceId info
  pure j

makeDictionaryFromVideoInfo :: (MonadJSM m) => MediaDeviceInfo -> m Dictionary
makeDictionaryFromVideoInfo info = liftJSM $ do
  videoConstraint <- mediaDeviceInfoToTrackConstraint info
  rawDic <- JS.obj
  rawDic <# "video" $ toJSVal videoConstraint
  rawDic <# "audio" $ True
  pure $ case rawDic of
           JS.Object val -> Dictionary val

makeDefaultUserMediaDictionary :: (MonadJSM m) => m Dictionary
makeDefaultUserMediaDictionary = liftJSM $ do
  rawDic <- JS.obj
  rawDic <# "video" $ True
  rawDic <# "audio" $ True
  case rawDic of
    JS.Object val -> do
      eval "console" ^. jsf "log" [val^.js "video"^. js0 "toString" ]
      pure $ Dictionary val


-- -- | <https://developer.mozilla.org/en-US/docs/Web/API/Navigator.webkitGetUserMedia Mozilla Navigator.webkitGetUserMedia documentation>
-- gonimoGetUserMedia' :: MonadDOM m => Navigator -> Maybe Dictionary -> m (Either NavigatorUserMediaError MediaStream)
-- gonimoGetUserMedia' self options = do
--     result <- liftIO newEmptyMVar
--     withCallback (newNavigatorUserMediaSuccessCallback (liftIO . putMVar result . Right . fromJust)) $ \success ->
--         withCallback (newNavigatorUserMediaErrorCallback (liftIO . putMVar result . Left . fromJust)) $ \error -> do
--             gonimoGetUserMediaRaw self options (Just success) (Just error)
--             liftIO $ takeMVar result

-- gonimoGetUserMedia :: MonadDOM m => Navigator -> Maybe Dictionary -> m MediaStream
-- gonimoGetUserMedia self options = gonimoGetUserMedia' self options >>= either throwUserMediaException return

-- gonimoGetUserMediaRaw :: (MonadDOM m, IsDictionary options) =>
--                       Navigator ->
--                       Maybe options ->
--                       Maybe NavigatorUserMediaSuccessCallback ->
--                       Maybe NavigatorUserMediaErrorCallback -> m ()
-- gonimoGetUserMediaRaw self options successCallback errorCallback = liftJSM $ do
--   self ^. js "mediaDevices"
--        ^. jsf "getUserMedia" [toJSVal options]
--        ^. jsf "then" [toJSVal successCallback]
--        ^. jsf "catch" [toJSVal errorCallback]
--   pure ()

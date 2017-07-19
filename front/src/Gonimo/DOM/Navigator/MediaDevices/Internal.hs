{-# LANGUAGE NoOverloadedStrings #-}

module Gonimo.DOM.Navigator.MediaDevices.Internal where


import           Control.Concurrent.MVar                     (newEmptyMVar,
                                                              putMVar, takeMVar)
import           GHCJS.DOM.Types                             (Dictionary (..),
                                                              fromJSValUnchecked, MediaStreamConstraints(..))
import           Language.Javascript.JSaddle                 (JSVal, MonadJSM,
                                                              eval, fun, js,
                                                              liftJSM,
                                                              ( # ), (<#))
import qualified Language.Javascript.JSaddle                 as JS

import           Gonimo.DOM.Navigator.MediaDevices.Types

import           Control.Lens                                ((^.))

import           Control.Monad.IO.Class                      (MonadIO (..))



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

makeConstraintsFromVideoInfo :: (MonadJSM m) => MediaDeviceInfo -> m MediaStreamConstraints
makeConstraintsFromVideoInfo info = liftJSM $ do
  videoConstraint <- mediaDeviceInfoToTrackConstraint info
  rawDic <- JS.obj
  rawDic <# "video" $ JS.toJSVal videoConstraint
  rawDic <# "audio" $ True
  pure $ case rawDic of
           JS.Object val -> MediaStreamConstraints val

makeSimpleMediaStreamConstraints :: (MonadJSM m) => Bool -> Bool -> m MediaStreamConstraints
makeSimpleMediaStreamConstraints audio video= liftJSM $ do
  rawDic <- JS.obj
  rawDic <# "audio" $ audio
  rawDic <# "video" $ video
  case rawDic of
    JS.Object val -> do
      -- eval "console" ^. jsf "log" [val^.js "video"^. js0 "toString" ]
      pure $ MediaStreamConstraints val


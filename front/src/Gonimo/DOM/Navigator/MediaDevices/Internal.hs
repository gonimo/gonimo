{-# LANGUAGE NoOverloadedStrings #-}

module Gonimo.DOM.Navigator.MediaDevices.Internal where


import           Control.Concurrent.MVar     (newEmptyMVar, putMVar, takeMVar)
import           GHCJS.Marshal               (fromJSValUnchecked)
import           Language.Javascript.JSaddle (JSVal, MonadJSM, eval, fun, js,
                                              js1, jsg, jss, liftJSM, syncPoint,
                                              valToNumber, ( # ))

import Gonimo.DOM.Navigator.MediaDevices.Types

import Control.Monad.IO.Class
import Control.Lens ((^.))

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

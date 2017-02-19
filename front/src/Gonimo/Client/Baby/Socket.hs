{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Gonimo.Client.Baby.Socket where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Maybe (catMaybes)
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Set                          ((\\))
import qualified Data.Set                          as Set
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Navigator               as Navigator
import qualified GHCJS.DOM.Window                  as Window
import qualified GHCJS.DOM.MediaStream             as MediaStream
import qualified GHCJS.DOM.MediaStreamTrack        as MediaStreamTrack
import           Gonimo.Client.Server              (webSocket_recv)
import qualified Gonimo.Client.Storage             as GStorage
import qualified Gonimo.Client.Storage.Keys        as GStorage
import qualified Gonimo.Db.Entities                as Db
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import           Reflex.Dom

import           GHCJS.DOM.Types                   (MediaStream, MonadJSM)
import qualified Gonimo.Client.App.Types           as App
import qualified Gonimo.Client.Auth                as Auth
import           Gonimo.Client.Subscriber          (SubscriptionsDyn)
import           Gonimo.DOM.Navigator.MediaDevices

data Config t
  = Config  { _configResponse :: Event t API.ServerResponse
            }

data Socket t
  = Socket { _videoDevices :: [MediaDeviceInfo]
            , _selectedCamera :: Dynamic t (Maybe Text)
            , _cameraEnabled :: Dynamic t Bool
            , _mediaStream :: Dynamic t MediaStream
            }


makeLenses ''Config
makeLenses ''Socket

socket :: forall m t. (MonadWidget t m)
        => Config t -> m (Socket t)
socket config = mdo
  let
    addChannel = push (\res -> do
                          cChannels <- sample $ current channels
                          case res of
                            API.EventChannelRequested fromId secret
                              -> pure . Just $ Map.insert (fromId, secret) Channel.emptyChannel
                            _ -> pure Nothing
                      )
                 (config^.configResponse)
    handleMessage = push (\res -> do
                          cChannels <- sample $ current channels
                          case res of
                            API.EventMessageReceived fromId secret msg
                              -> pure . Just $ Map.update Channel.handleMessage fromId secret msg
                             )
                 (config^.configResponse)
  
  channels <- holdDyn Map.empty 

handleCameraEnable :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> m (Dynamic t Bool)
handleCameraEnable config = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mLastEnabled <- GStorage.getItem storage GStorage.cameraEnabled

    let initVal = fromMaybe True $ mLastEnabled

    enabled <- holdDyn initVal (config^.configEnableCamera)
    performEvent_
      $ GStorage.setItem storage GStorage.CameraEnabled <$> updated enabled
    pure enabled

handleCameraSelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> [MediaDeviceInfo] -> m (Dynamic t (Maybe Text))
handleCameraSelect config devices = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mLastCameraLabel <- GStorage.getItem storage GStorage.selectedCamera

    let mLastUsedInfo = do
          lastCameraLabel <- mLastCameraLabel
          headMay . filter ((== lastCameraLabel) . mediaDeviceLabel) $ devices -- Only if still valid!

    let initVal = mediaDeviceLabel <$> (mLastUsedInfo <|> headMay devices)

    (initEv, trigger) <- newTriggerEvent
    liftIO $ trigger initVal -- We need to trigger a change for selecting the right MediaStream, we cannot sample current because this would result in a MonadFix loop.
    let selectEvent = leftmost [Just <$> config^.configSelectCamera, initEv]
    selected <- holdDyn initVal selectEvent
    performEvent_
      $ traverse_ (GStorage.setItem storage GStorage.selectedCamera) <$> updated selected

    pure selected

getMediaDeviceByLabel :: Text -> [MediaDeviceInfo] -> Maybe MediaDeviceInfo
getMediaDeviceByLabel label infos =
  let
    withLabel = filter ((== label) . mediaDeviceLabel) infos
  in
    headMay withLabel

getInitialMediaStream :: forall m t. (HasWebView m, MonadWidget t m)
                         => m MediaStream
getInitialMediaStream = do
  navigator <- Window.getNavigatorUnsafe =<< DOM.currentWindowUnchecked
  constr <- makeSimpleUserMediaDictionary True False
  Navigator.getUserMedia navigator $ Just constr

getConstrainedMediaStream :: forall m t. (MonadJSM m, Reflex t, MonadSample t m)
                         => Dynamic t MediaStream -> [MediaDeviceInfo] -> Maybe Text -> m MediaStream
getConstrainedMediaStream mediaStreams infos mLabel = do
  oldStream <- sample $ current mediaStreams
  stopMediaStream oldStream
  let mInfo = flip getMediaDeviceByLabel infos =<< mLabel
  navigator <- Window.getNavigatorUnsafe =<< DOM.currentWindowUnchecked
  constr <- case (mInfo, mLabel) of
    (_, Nothing)        -> makeSimpleUserMediaDictionary True False
    (Nothing, Just _)   -> makeSimpleUserMediaDictionary True True
    (Just info, Just _) -> makeDictionaryFromVideoInfo info
  Navigator.getUserMedia navigator $ Just constr


stopMediaStream :: forall m. MonadJSM m => MediaStream -> m ()
stopMediaStream stream = do
  tracks <- catMaybes <$> MediaStream.getTracks stream
  traverse_ MediaStreamTrack.stop tracks

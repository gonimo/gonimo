{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Gonimo.Client.Baby.Internal where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Maybe (catMaybes)
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Navigator               as Navigator
import qualified GHCJS.DOM.Window                  as Window
import qualified GHCJS.DOM.MediaStream             as MediaStream
import qualified GHCJS.DOM.MediaStreamTrack        as MediaStreamTrack
import qualified Gonimo.Client.Storage             as GStorage
import qualified Gonimo.Client.Storage.Keys        as GStorage
import qualified Gonimo.SocketAPI                  as API
import qualified Gonimo.SocketAPI.Types            as API
import           Reflex.Dom.Core

import           GHCJS.DOM.Types                   (MediaStream, MonadJSM)
import           Gonimo.DOM.Navigator.MediaDevices
import qualified Gonimo.Client.Baby.Socket         as Socket

data Config t
  = Config  { _configSelectCamera :: Event t Text
            , _configEnableCamera :: Event t Bool
            , _configResponse :: Event t API.ServerResponse
            , _configAuthData :: Dynamic t API.AuthData
            , _configStartMonitor  :: Event t ()
            , _configStopMonitor  :: Event t ()
            }

data Baby t
  = Baby { _videoDevices :: [MediaDeviceInfo]
         , _selectedCamera :: Dynamic t (Maybe Text)
         , _cameraEnabled :: Dynamic t Bool
         , _mediaStream :: Dynamic t MediaStream
         , _socket :: Socket.Socket t
         }

data UI t
  = UI { _uiGoHome :: Event t ()
       , _uiStartMonitor  :: Event t ()
       , _uiStopMonitor  :: Event t ()
       , _uiEnableCamera :: Event t Bool
       , _uiSelectCamera  :: Event t Text
       }

makeLenses ''Config
makeLenses ''Baby
makeLenses ''UI

uiSwitchPromptly :: forall t m. (MonadHold t m, Reflex t, MonadFix m) => Event t (UI t) -> m (UI t)
uiSwitchPromptly ev
  = UI <$> switchPromptly never (_uiGoHome <$> ev)
       <*> switchPromptly never (_uiStartMonitor <$> ev)
       <*> switchPromptly never (_uiStopMonitor <$> ev)
       <*> switchPromptly never (_uiEnableCamera <$> ev)
       <*> switchPromptly never (_uiSelectCamera <$> ev)

uiSwitchPromptlyDyn :: forall t. Reflex t => Dynamic t (UI t) -> UI t
uiSwitchPromptlyDyn ev
  = UI ( switchPromptlyDyn (_uiGoHome <$> ev) )
       ( switchPromptlyDyn (_uiStartMonitor <$> ev) )
       ( switchPromptlyDyn (_uiStopMonitor <$> ev) )
       ( switchPromptlyDyn (_uiEnableCamera <$> ev) )
       ( switchPromptlyDyn (_uiSelectCamera <$> ev) )

baby :: forall m t. (MonadWidget t m, HasWebView m)
        => Config t -> m (Baby t)
baby config = mdo
  badInit <- getInitialMediaStream -- IMPORTANT: This has to be before retrieving camera devices!
  devices <- enumerateDevices
  let videoDevices' = filter ((== VideoInput) . mediaDeviceKind) devices
  selected <- handleCameraSelect config devices
  enabled  <- handleCameraEnable config
  let mSelected = (\enabled' selected' -> if enabled' then selected' else Nothing)
                  <$> enabled <*> selected

  gotNewStream <- performEvent $ getConstrainedMediaStream mediaStream' videoDevices' <$> updated mSelected
  -- WARNING: Don't do that- -inifinte MonadFix loop will down on you!
  -- cSelected <- sample $ current selected
  mediaStream' <- holdDyn badInit gotNewStream

  sockEnabled <- holdDyn False $ leftmost [ const True <$> config^.configStartMonitor
                                          , const False <$> config^.configStopMonitor
                                          ]

  socket' <- Socket.socket $ Socket.Config { Socket._configResponse = config^.configResponse
                                           , Socket._configAuthData = config^.configAuthData
                                           , Socket._configEnabled = sockEnabled
                                           , Socket._configMediaStream = mediaStream'
                                           }

  pure $ Baby { _videoDevices = videoDevices'
              , _selectedCamera = selected
              , _cameraEnabled = enabled
              , _mediaStream = mediaStream'
              , _socket = socket'
              }

handleCameraEnable :: forall m t. (MonadHold t m, MonadJSM (Performable m), MonadJSM m, PerformEvent t m)
                      => Config t -> m (Dynamic t Bool)
handleCameraEnable config = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mLastEnabled <- GStorage.getItem storage GStorage.cameraEnabled

    let initVal = fromMaybe True $ mLastEnabled

    enabled <- holdDyn initVal (config^.configEnableCamera)
    performEvent_
      $ GStorage.setItem storage GStorage.CameraEnabled <$> updated enabled
    pure enabled

handleCameraSelect :: forall m t. (MonadHold t m, MonadJSM (Performable m), MonadJSM m, PerformEvent t m, TriggerEvent t m)
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

getInitialMediaStream :: forall m. MonadJSM m => m MediaStream
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

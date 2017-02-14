{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Gonimo.Client.Baby.Internal where

import Gonimo.Client.Prelude

import           Control.Lens
import           Data.Map                          (Map)
import qualified Data.Map                          as Map
import           Data.Set                          ((\\))
import qualified Data.Set                          as Set
import qualified GHCJS.DOM                         as DOM
import qualified GHCJS.DOM.Navigator               as Navigator
import qualified GHCJS.DOM.Window                  as Window
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
  = Config  { _configSelectCamera :: Event t Text
            }

data Baby t
  = Baby { _videoDevices :: [MediaDeviceInfo]
         , _selectedCamera :: Dynamic t Text
         , _mediaStream :: Dynamic t MediaStream
         }


makeLenses ''Config
makeLenses ''Baby

baby :: forall m t. (MonadWidget t m)
        => Config t -> m (Baby t)
baby config = do
  badInit <- getInitialMediaStream -- IMPORTANT: This has to be before retrieving camera devices!
  devices <- enumerateDevices
  let videoDevices' = filter ((== VideoInput) . mediaDeviceKind) devices
  selected <- handleCameraSelect config devices


  gotNewStream <- performEvent $ getConstrainedMediaStream videoDevices' <$> updated selected
  -- WARNING: Don't do that- -inifinte MonadFix loop will down over you! Or under!
  -- cSelected <- sample $ current selected
  mediaStream' <- holdDyn badInit gotNewStream

  pure $ Baby { _videoDevices = videoDevices'
              , _selectedCamera = selected
              , _mediaStream = mediaStream'
              }


handleCameraSelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> [MediaDeviceInfo] -> m (Dynamic t Text)
handleCameraSelect config devices = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mLastCameraLabel <- GStorage.getItem storage GStorage.selectedCamera

    let initVal = fromMaybe ""
                  $ mLastCameraLabel
                  <|> (mediaDeviceLabel <$> headMay devices)
    (initEv, trigger) <- newTriggerEvent
    liftIO $ trigger initVal -- We need to trigger a change for selecting the right MediaStream, we cannot sample current because this would result in a MonadFix loop.
    let selectEvent = leftmost [config^.configSelectCamera, initEv]
    selected <- holdDyn initVal selectEvent
    performEvent_
      $ GStorage.setItem storage GStorage.selectedCamera <$> updated selected

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
  constr <- makeDefaultUserMediaDictionary
  Navigator.getUserMedia navigator $ Just constr

getConstrainedMediaStream :: forall m. MonadJSM m
                         => [MediaDeviceInfo] -> Text -> m MediaStream
getConstrainedMediaStream infos label = do
  let mInfo = getMediaDeviceByLabel label infos
  navigator <- Window.getNavigatorUnsafe =<< DOM.currentWindowUnchecked
  constr <- case mInfo of
    Nothing -> makeDefaultUserMediaDictionary
    Just info -> makeDictionaryFromVideoInfo info
  Navigator.getUserMedia navigator $ Just constr

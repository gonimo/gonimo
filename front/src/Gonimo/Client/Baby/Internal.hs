{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Gonimo.Client.Baby.Internal where

import Gonimo.Client.Prelude

import Reflex.Dom
import qualified Gonimo.Db.Entities as Db
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set ((\\))
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import Control.Lens 
import qualified GHCJS.DOM.Window as Window
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM as DOM
import Gonimo.Client.Server (webSocket_recv)

import Gonimo.Client.Subscriber (SubscriptionsDyn)
import qualified Gonimo.Client.App.Types as App
import qualified Gonimo.Client.Auth as Auth
import Gonimo.DOM.Navigator.MediaDevices

data Config t
  = Config  { _configSelectCamera :: Event t Text
            }

data Baby t
  = Baby { _videoDevices :: [MediaDeviceInfo]
         , _selectedCamera :: Dynamic t Text
         }


makeLenses ''Config
makeLenses ''Baby

baby :: forall m t. (MonadWidget t m) => Config t -> m (Baby t)
baby config = do
  devices <- enumerateDevices
  let videoDevices' = filter ((== VideoInput) . mediaDeviceKind) devices
  selected <- handleCameraSelect config devices
  pure $ Baby { _videoDevices = videoDevices'
              , _selectedCamera = selected
              }


handleCameraSelect :: forall m t. (HasWebView m, MonadWidget t m)
                      => Config t -> [MediaDeviceInfo] -> m (Dynamic t Text)
handleCameraSelect config devices = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    mLastCameraLabel <- GStorage.getItem storage GStorage.selectedCamera


    let selectEvent = config^.configSelectCamera
    let initVal = fromMaybe ""
                  $ mLastCameraLabel
                  <|> (mediaDeviceLabel <$> headMay devices)
    selected <- holdDyn initVal selectEvent
    performEvent_
      $ GStorage.setItem storage GStorage.selectedCamera <$> updated selected

    pure selected

getMediaDeviceByLabel :: Text -> Baby t -> Maybe MediaDeviceInfo
getMediaDeviceByLabel label baby' =
  let
    devs = baby'^.videoDevices
    withLabel = filter ((== label) . mediaDeviceLabel) devs
  in
    headMay withLabel

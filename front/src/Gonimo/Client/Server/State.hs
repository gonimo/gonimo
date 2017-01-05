{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Retrieve state from server.
module Gonimo.Client.Server.State where

import Reflex
import Reflex.Dom
import Reflex.PerformEvent.Class (performEvent_)
import Control.Lens
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import qualified GHCJS.DOM as DOM
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM.JSFFI.Generated.Window as Window
import qualified GHCJS.DOM.JSFFI.Generated.Navigator as Navigator
import GHCJS.DOM.JSFFI.Generated.Storage (Storage)
import Data.Text (Text)
import Safe (headMay)
import Control.Monad.IO.Class
import Control.Monad (when)
import Data.Maybe (isNothing, isJust, catMaybes)
import Gonimo.Server.Db.Entities (FamilyId, Family, DeviceId)
import Gonimo.SocketAPI.Types (DeviceInfo, DeviceType)

-- data StateCommand = StateCreateDevice

data StateConfig t
  = StateConfig { _stateConfigResponse :: Event t API.ServerResponse
                , _stateConfigServerOpen :: Event t ()
                }

data State t
  = State { _stateRequest :: Event t [ API.ServerRequest ]
          , _stateCurrentFamilyId :: FamilyId
          , _stateFamilies :: Map FamilyId Family
          , _stateDeviceInfos :: Map DeviceId DeviceInfo
          , _stateOnlineDevices :: Map DeviceId DeviceType}
          }

makeLenses ''StateConfig
makeLenses ''State

state :: forall t m. (HasWebView m, MonadWidget t m) => StateConfig t -> m (State t)
state config = do
  (makeDeviceEvent, stateDataDyn) <- makeStateData config
  let stateenticateEvent = stateenticate config stateDataDyn
  pure $ State { _stateRequest = mconcat
                               . map (fmap (:[]))
                               $ [ makeDeviceEvent
                                 , stateenticateEvent
                                 ]
              }

makeStateData :: forall t m. (HasWebView m, MonadWidget t m)
  => StateConfig t -> m (Event t API.ServerRequest, Dynamic t (Maybe API.StateData))
makeStateData config = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked

    initial <- loadStateData storage
    makeDevice <- if isNothing initial
                  then Just . API.ReqMakeDevice . Just <$> getUserAgentString
                  else pure Nothing
    let makeDeviceEvent = push (pure . const makeDevice) $ config^.stateConfigServerOpen
    stateDataDyn <- holdDyn initial (Just <$> serverState)

    performEvent_
      $ writeStateData storage <$> updated stateDataDyn

    pure (makeDeviceEvent, stateDataDyn)
  where
    serverState :: Event t API.StateData
    serverState = push (pure . fromServerResponse) $ config^.stateConfigResponse

    fromServerResponse :: API.ServerResponse -> Maybe API.StateData
    fromServerResponse resp = case resp of
      API.ResMadeDevice state' -> Just state'
      _ -> Nothing


stateenticate :: forall t. Reflex t => StateConfig t -> Dynamic t (Maybe API.StateData) -> Event t API.ServerRequest
stateenticate config stateDataDyn=
  let
    stateDataList = catMaybes
                   <$> (mconcat . map (fmap (:[])))
                   [ tag (current stateDataDyn) $ config^.stateConfigServerOpen
                   , updated stateDataDyn
                   ]
    stateData = push (pure . headMay) stateDataList
  in
    API.ReqStateenticate . API.stateToken <$> stateData

writeStateData :: MonadIO m => Storage -> Maybe API.StateData -> m ()
writeStateData _ Nothing = pure ()
writeStateData storage (Just state') = GStorage.setItem storage GStorage.keyStateData state'


loadStateData :: MonadIO m => Storage -> m (Maybe API.StateData)
loadStateData storage = GStorage.getItem storage GStorage.keyStateData

getUserAgentString :: MonadIO m => m Text
getUserAgentString = do
  window  <- DOM.currentWindowUnchecked
  navigator <- Window.getNavigatorUnsafe window
  Navigator.getUserAgent navigator

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.Auth where

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
import qualified GHCJS.DOM.JSFFI.Generated.Location as Location
import GHCJS.DOM.JSFFI.Generated.Storage (Storage)
import GHCJS.DOM.Types (FromJSVal, fromJSVal, toJSString)
import Data.Text (Text)
import Safe (headMay)

import Control.Monad.IO.Class
import Control.Monad (when)
import Data.Maybe (isNothing, isJust, catMaybes)

-- data AuthCommand = AuthCreateDevice

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configServerOpen :: Event t ()
           }

data Auth t
  = Auth { _request :: Event t [ API.ServerRequest ]
         , _authData :: Dynamic t (Maybe API.AuthData)
         , _authenticated :: Event t ()
         }

makeLenses ''Config
makeLenses ''Auth

auth :: forall t m. (HasWebView m, MonadWidget t m) => Config t -> m (Auth t)
auth config = do
  (makeDeviceEvent, authDataDyn) <- makeAuthData config
  let authenticateEvent = authenticate config authDataDyn
  performEvent_
    $ handleStolenSession <$> config^.configResponse
  let
    authenticated' :: Event t ()
    authenticated' = do
      let handleAuthenticated resp = pure $ case resp of
            API.ResAuthenticated -> Just ()
            _                    -> Nothing
      push handleAuthenticated $ config^.configResponse

  pure $ Auth { _request = mconcat
                               . map (fmap (:[]))
                               $ [ makeDeviceEvent
                                 , authenticateEvent
                                 ]
              , _authData = authDataDyn
              , _authenticated = authenticated'
              }

makeAuthData :: forall t m. (HasWebView m, MonadWidget t m)
  => Config t -> m (Event t API.ServerRequest, Dynamic t (Maybe API.AuthData))
makeAuthData config = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked

    initial <- loadAuthData storage
    makeDevice <- if isNothing initial
                  then Just . API.ReqMakeDevice . Just <$> getUserAgentString
                  else pure Nothing
    let makeDeviceEvent = push (pure . const makeDevice) $ config^.configServerOpen
    authDataDyn <- holdDyn initial (Just <$> serverAuth)

    performEvent_
      $ writeAuthData storage <$> updated authDataDyn

    pure (makeDeviceEvent, authDataDyn)
  where
    serverAuth :: Event t API.AuthData
    serverAuth = push (pure . fromServerResponse) $ config^.configResponse

    fromServerResponse :: API.ServerResponse -> Maybe API.AuthData
    fromServerResponse resp = case resp of
      API.ResMadeDevice auth' -> Just auth'
      _ -> Nothing


authenticate :: forall t. Reflex t => Config t -> Dynamic t (Maybe API.AuthData) -> Event t API.ServerRequest
authenticate config authDataDyn=
  let
    authDataList = catMaybes
                   <$> (mconcat . map (fmap (:[])))
                   [ tag (current authDataDyn) $ config^.configServerOpen
                   , updated authDataDyn
                   ]
    authData = push (pure . headMay) authDataList
  in
    API.ReqAuthenticate . API.authToken <$> authData

writeAuthData :: MonadIO m => Storage -> Maybe API.AuthData -> m ()
writeAuthData _ Nothing = pure ()
writeAuthData storage (Just auth') = GStorage.setItem storage GStorage.keyAuthData auth'

handleStolenSession :: MonadIO m => API.ServerResponse -> m ()
handleStolenSession API.EventSessionGotStolen = do
  window  <- DOM.currentWindowUnchecked
  location <- Window.getLocationUnsafe window
  Location.setPathname location ("/stolenSession.html" :: Text)
handleStolenSession _ = pure ()


loadAuthData :: MonadIO m => Storage -> m (Maybe API.AuthData)
loadAuthData storage = GStorage.getItem storage GStorage.keyAuthData

getUserAgentString :: MonadIO m => m Text
getUserAgentString = do
  window  <- DOM.currentWindowUnchecked
  navigator <- Window.getNavigatorUnsafe window
  Navigator.getUserAgent navigator


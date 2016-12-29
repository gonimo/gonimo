{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Text (Text)

import Control.Monad.IO.Class
import Control.Monad (when)
import Data.Maybe (isNothing)

-- data AuthCommand = AuthCreateDevice

data AuthConfig t
  = AuthConfig { _authConfigResponse :: Event t API.ServerResponse
               -- , _authCommand :: Event t AuthCommand
               }

data Auth t
  = Auth { _authRequest :: Event t API.ServerRequest
         , _authData :: Dynamic t (Maybe API.AuthData)
         }

makeLenses ''AuthConfig
makeLenses ''Auth

auth :: forall t m. (HasWebView m, MonadWidget t m) => AuthConfig t -> m (Auth t)
auth config = do
  window  <- DOM.currentWindowUnchecked
  storage <- Window.getLocalStorageUnsafe window
  mAuth   <- GStorage.getItem storage GStorage.keyAuthData
  navigator <- Window.getNavigatorUnsafe window
  mUserAgent :: Maybe Text <- Just <$> Navigator.getUserAgent navigator

  let
    writeLocalStorage :: MonadIO m1 => Maybe API.AuthData -> m1 ()
    writeLocalStorage Nothing = pure ()
    writeLocalStorage (Just auth') = GStorage.setItem storage GStorage.keyAuthData auth'

  (mAuthInitEvent, triggerEvent) <- newTriggerEvent

  when (isNothing mAuth)
    $ liftIO $ triggerEvent (API.ReqMakeDevice mUserAgent)

  authDataDyn <- makeAuthData mAuth config

  performEvent_ $ fmap writeLocalStorage (updated authDataDyn)

  pure $ Auth { _authRequest = mAuthInitEvent
              , _authData = authDataDyn
              }

makeAuthData :: forall t m. (MonadHold t m, Reflex t) => Maybe API.AuthData -> AuthConfig t -> m (Dynamic t (Maybe API.AuthData))
makeAuthData initial config = do
    let
      fromServerResponse :: API.ServerResponse -> Maybe API.AuthData
      fromServerResponse resp = case resp of
        API.ResMakeDevice auth' -> Just auth'
        _ -> Nothing

      serverAuth :: Event t API.AuthData
      serverAuth = push (pure . fromServerResponse) $ config^.authConfigResponse

    holdDyn initial (fmap Just serverAuth)


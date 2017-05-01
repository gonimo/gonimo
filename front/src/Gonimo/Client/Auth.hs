{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gonimo.Client.Auth where

import Reflex
import Reflex.Dom.Core
import Reflex.PerformEvent.Class (performEvent_)
import Control.Lens
import qualified Gonimo.SocketAPI.Types as API
import qualified Gonimo.SocketAPI as API
import qualified GHCJS.DOM as DOM
import qualified Gonimo.Client.Storage as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Navigator as Navigator
import qualified GHCJS.DOM.Location as Location
import GHCJS.DOM.Storage (Storage)
import Data.Text (Text)
import Safe (headMay)
import           GHCJS.DOM.Types (MonadJSM)
import qualified Data.Text as T
import Data.Time.Clock

import Control.Monad.IO.Class
import Data.Maybe (isNothing, catMaybes)
import Gonimo.Client.Prelude
import Gonimo.Client.Auth.I18N

-- data AuthCommand = AuthCreateDevice

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configServerOpen :: Event t ()
           , _configServerClose :: Event t ()
           }

data Auth t
  = Auth { _request :: Event t [ API.ServerRequest ]
         , _authData :: Dynamic t (Maybe API.AuthData)
         , _authenticated :: Event t () -- TODO: Now redundant, because of isOnline.
         , _isOnline :: Dynamic t Bool
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
  isOnline' <- holdDyn False $ leftmost [ const False <$> config^.configServerClose
                                        , const True <$> authenticated'
                                        ]

  pure $ Auth { _request = mconcat
                               . map (fmap (:[]))
                               $ [ makeDeviceEvent
                                 , authenticateEvent
                                 ]
              , _authData = authDataDyn
              , _authenticated = authenticated'
              , _isOnline = isOnline'
              }

makeAuthData :: forall t m. (HasWebView m, MonadWidget t m)
  => Config t -> m (Event t API.ServerRequest, Dynamic t (Maybe API.AuthData))
makeAuthData config = do
    storage <- Window.getLocalStorageUnsafe =<< DOM.currentWindowUnchecked
    userAgentString <- getUserAgentString

    initial <- loadAuthData storage
    authDataDyn <- holdDyn initial (Just <$> serverAuth)
    let makeDevice = do
          cAuth <- sample $ current authDataDyn
          if isNothing cAuth
          then pure . Just . API.ReqMakeDevice . Just $ userAgentString
          else pure Nothing
    let makeDeviceEvent = push (const makeDevice) $ config^.configServerOpen

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
    authData' = push (pure . headMay) authDataList
  in
    API.ReqAuthenticate . API.authToken <$> authData'

writeAuthData :: MonadJSM m => Storage -> Maybe API.AuthData -> m ()
writeAuthData _ Nothing = pure ()
writeAuthData storage (Just auth') = GStorage.setItem storage GStorage.keyAuthData auth'

handleStolenSession :: MonadJSM m => API.ServerResponse -> m ()
handleStolenSession API.EventSessionGotStolen = do
  window  <- DOM.currentWindowUnchecked
  location <- Window.getLocationUnsafe window
  Location.setPathname location ("/stolenSession.html" :: Text)
handleStolenSession _ = pure ()


loadAuthData :: MonadJSM m => Storage -> m (Maybe API.AuthData)
loadAuthData storage = GStorage.getItem storage GStorage.keyAuthData

getUserAgentString :: MonadJSM m => m Text
getUserAgentString = do
  window  <- DOM.currentWindowUnchecked
  navigator <- Window.getNavigatorUnsafe window
  Navigator.getUserAgent navigator


connectionLossScreen :: forall m t. GonimoM t m
  => Auth t -> m ()
connectionLossScreen auth' = do
  _ <- dyn $ connectionLossScreen' . not <$> auth'^.isOnline
  pure ()

connectionLossScreen' :: forall m t. GonimoM t m
  => Bool -> m ()
connectionLossScreen' isBroken = case isBroken of
  False -> pure ()
  True  -> elClass "div" "notification overlay" $ do
    elClass "div" "notification box connection-lost" $ do
      elClass "div" "notification-header" $ do
        el "h1" $ trText No_Internet_Connection
      elClass "div" "notification text" $ do
        trText Reconnecting
        dynText =<< loadingDots
        el "br" blank
        elClass "div" "welcome-container" $
          elClass "div" "start-welcome-img" $ blank

loadingDots :: forall m t. GonimoM t m => m (Dynamic t Text)
loadingDots = do
  now <- liftIO $ getCurrentTime
  tick <- tickLossy 1 now
  tickCount :: Dynamic t Int <- count tick
  let dotCount = mod <$> tickCount <*> pure 8
  pure $ T.replicate <$> dotCount <*> pure "."

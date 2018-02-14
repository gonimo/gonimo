{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Gonimo.Client.Auth ( -- * User interface
                            module Gonimo.Client.Auth.API
                            -- * Other types and classes
                          , FullAuth(..)
                          , serverConfig
                          , _auth
                            -- * Creation
                          , make
                          -- * Utility functions
                          -- TODO: Those should really not belong here - right?!
                          , connectionLossScreen
                          , loadingDots
                          ) where

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
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.NavigatorID as Navigator
import qualified GHCJS.DOM.Location as Location
import GHCJS.DOM.Storage (Storage)
import Data.Text (Text)
import           GHCJS.DOM.Types (MonadJSM)
import qualified Data.Text as T
import Data.Time.Clock

import Control.Monad.IO.Class
import Gonimo.Client.Prelude
import Gonimo.Client.Auth.I18N
import Gonimo.I18N
import Gonimo.Client.Auth.API
import Gonimo.Client.Server as Server hiding (make)

-- data AuthCommand = AuthCreateDevice

data FullAuth t
  = FullAuth { _serverConfig :: Server.Config t
             , __auth :: Auth t
             }

make :: forall c t m. (HasWebView m, MonadWidget t m, Server.HasServer c, HasConfig c)
  => Dynamic t Locale -> c t -> m (FullAuth t)
make locDyn conf = do
  (makeDeviceEvent, _authData) <- makeAuthData conf
  let authenticateEvent = authenticate conf _authData
  performEvent_
    $ handleStolenSession <$> attach (current locDyn) (conf^.Server.onResponse)
  let
    _onAuthenticated :: Event t ()
    _onAuthenticated = do
      let handleAuthenticated resp = pure $ case resp of
            API.ResAuthenticated -> Just ()
            _                    -> Nothing
      push handleAuthenticated $ conf^.Server.onResponse
  _isOnline <- fmap uniqDyn
               . holdDyn False
               $ leftmost [ const False <$> conf^.Server.onClose
                          , const False <$> conf^.Server.onCloseRequested
                          , const True <$> _onAuthenticated
                          ]
  let _onRequest = mconcat
                   . map (fmap (:[]))
                   $ [ makeDeviceEvent
                     , authenticateEvent
                     ]


  pure $ FullAuth { _serverConfig = Server.Config {..}
                  , __auth = Auth {..}
                  }

makeAuthData :: forall c t m. (HasWebView m, MonadWidget t m, Server.HasServer c)
  => c t -> m (Event t API.ServerRequest, Dynamic t (Maybe API.AuthData))
makeAuthData conf = do
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
    userAgentString <- getUserAgentString

    initial <- loadAuthData storage
    authDataDyn <- holdDyn initial (Just <$> serverAuth)
    let makeDevice = do
          cAuth <- sample $ current authDataDyn
          if isNothing cAuth
          then pure . Just . API.ReqMakeDevice . Just $ userAgentString
          else pure Nothing
    let makeDeviceEvent = push (const makeDevice) $ conf^.Server.onOpen

    performEvent_
      $ writeAuthData storage <$> updated authDataDyn

    pure (makeDeviceEvent, authDataDyn)
  where
    serverAuth :: Event t API.AuthData
    serverAuth = push (pure . fromServerResponse) $ conf^.Server.onResponse

    fromServerResponse :: API.ServerResponse -> Maybe API.AuthData
    fromServerResponse resp = case resp of
      API.ResMadeDevice auth' -> Just auth'
      _ -> Nothing


authenticate :: forall c t. (Reflex t, HasServer c) => c t -> Dynamic t (Maybe API.AuthData) -> Event t API.ServerRequest
authenticate conf authDataDyn =
  let
    authDataList = leftmost
                    [ tag (current authDataDyn) $ conf^.Server.onOpen
                    , updated authDataDyn
                    ]
    authData' = push pure authDataList
  in
    API.ReqAuthenticate . API.authToken <$> authData'

writeAuthData :: MonadJSM m => Storage -> Maybe API.AuthData -> m ()
writeAuthData _ Nothing = pure ()
writeAuthData storage (Just auth') = GStorage.setItem storage GStorage.keyAuthData auth'

handleStolenSession :: MonadJSM m => (Locale, API.ServerResponse) -> m ()
handleStolenSession (loc , API.EventSessionGotStolen) = do
  doc  <- DOM.currentDocumentUnchecked
  location <- Document.getLocationUnsafe doc
  case loc of
    EN_GB -> Location.setPathname location ("/stolenSession.html" :: Text)
    DE_DE -> Location.setPathname location ("/stolenSession-de.html" :: Text)
handleStolenSession _ = pure ()


loadAuthData :: MonadJSM m => Storage -> m (Maybe API.AuthData)
loadAuthData storage = GStorage.getItem storage GStorage.keyAuthData

getUserAgentString :: MonadJSM m => m Text
getUserAgentString = do
  window  <- DOM.currentWindowUnchecked
  navigator <- Window.getNavigator window
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


-- Auto generated lenses:

-- Lenses for FullAuth t:

serverConfig :: Lens' (FullAuth t) (Server.Config t)
serverConfig f fullAuth' = (\serverConfig' -> fullAuth' { _serverConfig = serverConfig' }) <$> f (_serverConfig fullAuth')

_auth :: Lens' (FullAuth t) (Auth t)
_auth f fullAuth' = (\_auth' -> fullAuth' { __auth = _auth' }) <$> f (__auth fullAuth')



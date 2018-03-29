{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gonimo.Client.Auth.Impl ( -- * User interface
                            module Gonimo.Client.Auth
                            -- * Other types and classes
                          , Model
                          , ModelConfig
                          , HasModel
                          , HasModelConfig
                            -- * Creation
                          , make
                          -- * Utility functions
                          -- TODO: Those should really not belong here - right?!
                          , connectionLossScreen
                          , connectionLossScreen'
                          , loadingDots
                          ) where

import           Control.Lens
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Clock
import qualified GHCJS.DOM                  as DOM
import qualified GHCJS.DOM.Document         as Document
import qualified GHCJS.DOM.Location         as Location
import qualified GHCJS.DOM.NavigatorID      as Navigator
import           GHCJS.DOM.Storage          (Storage)
import           GHCJS.DOM.Types            (MonadJSM)
import qualified GHCJS.DOM.Window           as Window
import qualified Gonimo.Client.Storage      as GStorage
import qualified Gonimo.Client.Storage.Keys as GStorage
import qualified Gonimo.SocketAPI           as API
import qualified Gonimo.SocketAPI.Types     as API
import           Reflex
import           Reflex.Dom.Core
import           Reflex.PerformEvent.Class  (performEvent_)

import           Control.Monad.IO.Class
import           Gonimo.Client.Auth
import           Gonimo.Client.Auth.I18N
import           Gonimo.Client.Model
import           Gonimo.Client.Prelude
import           Gonimo.Client.Server       as Server hiding (make, HasModel, Model)
import qualified Gonimo.Client.Settings     as Settings
import           Gonimo.I18N

-- | Simple data type fulfilling our 'HasModel' dependencies.
type Model t = Server t

-- | Our dependencies
type HasModel model = (Server.HasServer model, Settings.HasSettings model)

-- | Example datatype fulfilling 'HasModelConfig'.
type ModelConfig t = Server.Config t

-- | Configurations we provide for the model as inputs.
type HasModelConfig c t = (IsConfig c t, Server.HasConfig c)

make :: forall model mConf t m
  . ( MonadHold t m, MonadJSM m, PerformEvent t m, MonadSample t m, MonadFix m, MonadJSM (Performable m)
    , HasModel model, HasModelConfig mConf t
    )
     => model t -> m (mConf t, Auth t)
make model = do
  (makeDeviceEvent, _authData) <- makeAuthData model
  let
    authenticateEvent = authenticate model _authData
    locDyn = model ^. Settings.locale

  performEvent_
    $ handleStolenSession <$> attach (current locDyn) (model^.Server.onResponse)
  let
    _onAuthenticated :: Event t ()
    _onAuthenticated = do
      let handleAuthenticated resp = pure $ case resp of
            API.ResAuthenticated -> Just ()
            _                    -> Nothing
      push handleAuthenticated $ model^.Server.onResponse
  _isOnline <- holdDynUniq False
               $ leftmost [ const False <$> model^.Server.onClose
                          , const False <$> model^.Server.onCloseRequested
                          , const True <$> _onAuthenticated
                          ]
  let auth' = Auth {..}
  let mConf = mempty & onRequest .~ mergeAsList [ makeDeviceEvent
                                                , authenticateEvent
                                                ]


  pure (mConf, auth')

makeAuthData :: forall d t m. (MonadHold t m, MonadJSM m, MonadSample t m, PerformEvent t m
                              , MonadJSM (Performable m)
                              , Server.HasServer d
                              )
  => d t -> m (Event t API.ServerRequest, Dynamic t (Maybe API.AuthData))
makeAuthData model = do
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
    userAgentString <- getUserAgentString

    initial <- loadAuthData storage
    authDataDyn <- holdDyn initial (Just <$> serverAuth)
    let makeDevice = do
          cAuth <- sample $ current authDataDyn
          if isNothing cAuth
          then pure . Just . API.ReqMakeDevice . Just $ userAgentString
          else pure Nothing
    let makeDeviceEvent = push (const makeDevice) $ model^.Server.onOpen

    performEvent_
      $ writeAuthData storage <$> updated authDataDyn

    pure (makeDeviceEvent, authDataDyn)
  where
    serverAuth :: Event t API.AuthData
    serverAuth = push (pure . fromServerResponse) $ model^.Server.onResponse

    fromServerResponse :: API.ServerResponse -> Maybe API.AuthData
    fromServerResponse resp = case resp of
      API.ResMadeDevice auth' -> Just auth'
      _                       -> Nothing


authenticate :: forall d t. (Reflex t, HasServer d) => d t -> Dynamic t (Maybe API.AuthData) -> Event t API.ServerRequest
authenticate model authDataDyn =
  let
    onAuth = leftmost
             [ tag (current authDataDyn) $ model^.Server.onOpen
             , updated authDataDyn
             ]
    authData' = push pure onAuth
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


connectionLossScreen :: forall a model m t. (GonimoM model t m, HasAuth a)
  => a t -> m ()
connectionLossScreen auth' = do
  _ <- dyn $ connectionLossScreen' . not <$> auth'^.isOnline
  pure ()

connectionLossScreen' :: forall model m t. GonimoM model t m
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

loadingDots :: forall model m t. GonimoM model t m => m (Dynamic t Text)
loadingDots = do
  now <- liftIO $ getCurrentTime
  tick <- tickLossy 1 now
  tickCount :: Dynamic t Int <- count tick
  let dotCount = mod <$> tickCount <*> pure 8
  pure $ T.replicate <$> dotCount <*> pure "."

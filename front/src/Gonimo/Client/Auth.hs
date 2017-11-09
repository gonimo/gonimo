{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- data AuthCommand = AuthCreateDevice

data Config t
  = Config { _configResponse :: Event t API.ServerResponse
           , _configServerOpen :: Event t ()
           , _configServerClose :: Event t ()
           , _configServerCloseRequested :: Event t ()
           }

data Auth t
  = Auth { _request :: Event t [ API.ServerRequest ]
         , _authData :: Dynamic t (Maybe API.AuthData)
         , _authenticated :: Event t () -- TODO: Now redundant, because of isOnline.
         , _isOnline :: Dynamic t Bool
         }

auth :: forall t m. (HasWebView m, MonadWidget t m) => Dynamic t Locale -> Config t -> m (Auth t)
auth locDyn config = do
  (makeDeviceEvent, authDataDyn) <- makeAuthData config
  let authenticateEvent = authenticate config authDataDyn
  performEvent_
    $ handleStolenSession <$> attach (current locDyn) (config^.configResponse)
  let
    authenticated' :: Event t ()
    authenticated' = do
      let handleAuthenticated resp = pure $ case resp of
            API.ResAuthenticated -> Just ()
            _                    -> Nothing
      push handleAuthenticated $ config^.configResponse
  isOnline' <- fmap uniqDyn
               . holdDyn False
               $ leftmost [ const False <$> config^.configServerClose
                          , const False <$> config^.configServerCloseRequested
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
    storage <- Window.getLocalStorage =<< DOM.currentWindowUnchecked
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
authenticate config authDataDyn =
  let
    authDataList = leftmost
                    [ tag (current authDataDyn) $ config^.configServerOpen
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


-- Lenses for Config t:

configResponse :: Lens' (Config t) (Event t API.ServerResponse)
configResponse f config' = (\configResponse' -> config' { _configResponse = configResponse' }) <$> f (_configResponse config')

configServerOpen :: Lens' (Config t) (Event t ())
configServerOpen f config' = (\configServerOpen' -> config' { _configServerOpen = configServerOpen' }) <$> f (_configServerOpen config')

configServerClose :: Lens' (Config t) (Event t ())
configServerClose f config' = (\configServerClose' -> config' { _configServerClose = configServerClose' }) <$> f (_configServerClose config')

configServerCloseRequested :: Lens' (Config t) (Event t ())
configServerCloseRequested f config' = (\configServerCloseRequested' -> config' { _configServerCloseRequested = configServerCloseRequested' }) <$> f (_configServerCloseRequested config')


-- Lenses for Auth t:

request :: Lens' (Auth t) (Event t [ API.ServerRequest ])
request f auth' = (\request' -> auth' { _request = request' }) <$> f (_request auth')

authData :: Lens' (Auth t) (Dynamic t (Maybe API.AuthData))
authData f auth' = (\authData' -> auth' { _authData = authData' }) <$> f (_authData auth')

authenticated :: Lens' (Auth t) (Event t ())
authenticated f auth' = (\authenticated' -> auth' { _authenticated = authenticated' }) <$> f (_authenticated auth')

isOnline :: Lens' (Auth t) (Dynamic t Bool)
isOnline f auth' = (\isOnline' -> auth' { _isOnline = isOnline' }) <$> f (_isOnline auth')


